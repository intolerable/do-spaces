{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Network.DO.Spaces.Request
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low-level implementations of Spaces REST transactions. You should not import
-- this module directly, but should instead use the higher-level interface exposed
-- by "Network.DO.Spaces.Actions" and its submodules
--
module Network.DO.Spaces.Request
    ( newSpacesRequest
    , mkSignature
    , mkStringToSign
    , mkAuthorization
    , finalize
    ) where

import           Control.Monad.Catch             ( MonadThrow )

import           Crypto.Hash                     ( SHA256, hashlazy )
import           Crypto.MAC.HMAC                 ( hmac )

import           Data.Bifunctor                  ( first )
import           Data.ByteArray                  ( convert )
import           Data.ByteString                 ( ByteString )
import qualified Data.ByteString.Base16          as B16
import qualified Data.ByteString.Char8           as C
import qualified Data.ByteString.Lazy            as LB
import qualified Data.CaseInsensitive            as CI
import           Data.Coerce                     ( coerce )
import           Data.Function                   ( (&) )
import           Data.Generics.Product           ( HasField(field) )
import           Data.Generics.Product.Positions ( HasPosition(position) )
import           Data.List                       ( sort )
import           Data.Maybe                      ( fromMaybe )
import qualified Data.Text                       as T
import           Data.Time
                 ( UTCTime
                 , defaultTimeLocale
                 , formatTime
                 )

import           Lens.Micro                      ( (^.) )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils
                 ( bodyLBS
                 , regionSlug
                 , toLowerBS
                 )
import           Network.HTTP.Client.Conduit     ( Request
                                                 , RequestBody(RequestBodyLBS)
                                                 )
import qualified Network.HTTP.Client.Conduit     as H
import           Network.HTTP.Types              ( Header, Query, QueryItem )
import qualified Network.HTTP.Types              as H

-- | Extract the 'Request' from a 'SpacesRequest' and set the requisite
-- @Authorization@ header
finalize :: SpacesRequest -> Authorization -> Request
finalize sr auth = req { H.requestHeaders = authHeader : reqHeaders }
  where
    authHeader = (CI.mk "authorization", uncompute auth)

    req        = sr ^. field @"request"

    reqHeaders = req & H.requestHeaders

-- | Create a new 'SpacesRequest' from a 'SpacesRequestBuilder', performing the
-- necessary computations and setting the appropriate default headers
newSpacesRequest
    :: MonadThrow m => SpacesRequestBuilder -> UTCTime -> m SpacesRequest
newSpacesRequest SpacesRequestBuilder { .. } time = do
    req <- H.parseRequest
        $ mconcat [ show reqMethod
                  , " "
                  , "https://"
                  , maybe mempty ((<> ".") . T.unpack . coerce) bucket
                  , regionSlug
                    $ fromMaybe (spaces ^. field @"region") overrideRegion
                  , "."
                  , "digitaloceanspaces.com/"
                  , maybe mempty (T.unpack . coerce) object
                  ]
    payload <- bodyLBS reqBody
    let payloadHash      = hashHex payload
        newHeaders       = overrideReqHeaders req payloadHash time
        request          = req
            { H.requestHeaders = headers <> newHeaders
            , H.queryString    =
                  maybe mempty (H.renderQuery True) subresources
                  <> maybe mempty (H.renderQuery False) queryString
            , H.requestBody    = reqBody
            }
        canonicalRequest = mkCanonicalized (fromMaybe mempty subresources)
                                           (fromMaybe mempty queryString)
                                           request
                                           payloadHash
    return
        $ SpacesRequest
        { method = reqMethod, headers = headers <> newHeaders, .. }
  where
    reqMethod = fromMaybe GET method

    reqBody   = fromMaybe (RequestBodyLBS LB.empty) body

-- | Canonicalize a 'Request'
mkCanonicalized :: Query -- ^ Subresources
                -> Query  -- ^ Query string
                -> Request
                -> Hashed  -- ^ The hashed 'RequestBody'
                -> Canonicalized Request
mkCanonicalized subresources query request payloadHash = Canonicalized
    $ C.intercalate "\n"
                    [ request & H.method
                    , request & H.path
                    , renderSubresources subresources
                      <> H.renderQuery False query
                    , request
                      & H.requestHeaders
                      & canonicalizeHeaders
                      & unCanonicalized
                    , request & H.requestHeaders & joinHeaderNames
                    , uncompute payloadHash
                    ]

-- | This is required to encode the subresources query string correctly in the
-- canonical request. Empty query keys require a trailing @=@, which are not
-- included with 'H.renderQuery'
renderSubresources :: Query -> ByteString
renderSubresources = C.intercalate "&" . fmap renderQueryItem . sort
  where
    renderQueryItem :: QueryItem -> ByteString
    renderQueryItem (k, Nothing) = k <> "="
    renderQueryItem (k, Just v)  = k <> "=" <> v

-- | Generate a 'StringToSign'
mkStringToSign :: SpacesRequest -> StringToSign
mkStringToSign req@SpacesRequest { .. } = StringToSign
    $ C.intercalate "\n"
                    [ "AWS4-HMAC-SHA256"
                    , fmtAmzTime time
                    , mkCredentials req & uncompute
                    , canonicalRequest
                      & coerce
                      & LB.fromStrict
                      & hashHex
                      & uncompute
                    ]

-- | Generate a 'Signature'
mkSignature :: SpacesRequest -> StringToSign -> Signature
mkSignature SpacesRequest { .. } str = Signature
    . B16.encode
    . keyedHash (uncompute str)
    . keyedHash "aws4_request"
    . keyedHash "s3"
    . keyedHash (spaces ^. field @"region" & regionSlug)
    . keyedHash (fmtAmzDate time)
    $ "AWS4" <> (spaces ^. field @"secretKey" & coerce)

-- | Create an 'Authorization' corresponding to the required AWS v4
-- @Authorization@ header
mkAuthorization :: SpacesRequest -> StringToSign -> Authorization
mkAuthorization req@SpacesRequest { .. } str = Authorization
    $ C.concat [ "AWS4-HMAC-SHA256 Credential="
                 <> spaces ^. field @"accessKey" . position @1
                 <> "/"
                 <> uncompute cred
               , ", SignedHeaders=" <> joinHeaderNames headers
               , ", Signature=" <> uncompute sig
               ]
  where
    cred = mkCredentials req

    sig  = mkSignature req str

-- | Create 'Credentials' containing your 'AccessKey' and the request 'Region'
mkCredentials :: SpacesRequest -> Credentials
mkCredentials SpacesRequest { .. } = Credentials
    $ C.intercalate "/"
                    [ fmtAmzDate time
                    , spaces ^. field @"region" & regionSlug
                    , "s3"
                    , "aws4_request"
                    ]

-- | Required to override @http-client@ automatically setting the Content-Length header and
-- setting obligatory headers for AWS v4 API requests
overrideReqHeaders
    :: Request
    -> Hashed   -- ^ The SHA256 hash of the request body; required in AWS v4
    -> UTCTime
    -> [Header]
overrideReqHeaders req hb time = (req & H.requestHeaders) <> newHeaders
  where
    newHeaders = [ (CI.mk "host", req & H.host)
                 , (CI.mk "x-amz-content-sha256", uncompute hb)
                 , (CI.mk "x-amz-date", fmtAmzTime time)
                 ]

-- | Canonicalize @['Header']@s
canonicalizeHeaders :: [Header] -> Canonicalized [Header]
canonicalizeHeaders = Canonicalized
    . C.unlines
    . fmap (\(n, v) -> n <> ":" <> v)
    . sort
    . fmap (first (toLowerBS . CI.original))

joinHeaderNames :: [Header] -> ByteString
joinHeaderNames =
    C.intercalate ";" . sort . fmap (toLowerBS . CI.original . fst)

fmtAmzDate :: UTCTime -> ByteString
fmtAmzDate = C.pack . formatTime defaultTimeLocale "%Y%m%d"

fmtAmzTime :: UTCTime -> ByteString
fmtAmzTime = C.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

keyedHash :: ByteString -> ByteString -> ByteString
keyedHash bs k = convert $ hmac @_ @_ @SHA256 k bs

hashHex :: LB.ByteString -> Hashed
hashHex = Hashed . B16.encode . convert . hashlazy @SHA256
