{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces.Request
    ( newSpacesRequest
    , mkSignature
    , mkStringToSign
    , mkAuthorization
    , finalize
    ) where

import           Control.Monad.Catch         ( MonadThrow(throwM) )

import           Crypto.Hash                 ( SHA256, hash )
import           Crypto.MAC.HMAC             ( hmac )

import           Data.Bifunctor              ( first )
import           Data.ByteArray              ( convert )
import           Data.ByteString             ( ByteString )
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import qualified Data.CaseInsensitive        as CI
import           Data.Function               ( (&) )
import           Data.List                   ( sort )
import           Data.Maybe                  ( fromMaybe )
import qualified Data.Text                   as T
import           Data.Time
                 ( UTCTime
                 , defaultTimeLocale
                 , formatTime
                 )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils     ( regionSlug, toLowerBS )
import           Network.HTTP.Client.Conduit
                 ( Request
                 , RequestBody(RequestBodyLBS, RequestBodyBS)
                 )
import qualified Network.HTTP.Client.Conduit as H
import           Network.HTTP.Types          ( Header )
import qualified Network.HTTP.Types          as H

-- | Extrac the 'Request' from a 'SpacesRequest' and set the requisite
-- @Authorization@ header
finalize :: SpacesRequest -> Authorization -> Request
finalize sr auth = req { H.requestHeaders = authHeader : reqHeaders }
  where
    authHeader = (CI.mk "authorization", uncompute auth)

    req        = sr & request

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
                  , maybe mempty ((<> ".") . T.unpack . unBucket) bucket
                  , regionSlug region
                  , "."
                  , "digitaloceanspaces.com/"
                  , maybe mempty (T.unpack . unObject) object
                  ]
    payload <- bodyBS $ fromMaybe (RequestBodyBS mempty) body
    let payloadHash      = hashHex payload
        newHeaders       = overrideReqHeaders req payloadHash time
        request          = req
            { H.requestHeaders = headers <> newHeaders
            , H.queryString    = maybe mempty (H.renderQuery True) queryString
            }
        canonicalRequest = mkCanonicalized request payloadHash
    return
        $ SpacesRequest
        { method = reqMethod, headers = headers <> newHeaders, .. }
  where
    Spaces { .. } = spaces

    reqMethod     = fromMaybe GET method

-- | Canonicalize a 'Request'
mkCanonicalized :: Request
                -> Hashed  -- ^ The hashed 'RequestBody'
                -> Canonicalized Request
mkCanonicalized request payloadHash = Canonicalized
    $ C.intercalate "\n"
                    [ request & H.method
                    , request & H.path
                      -- TODO fix query params, strip leading '?'
                    , request & H.queryString
                    , request
                      & H.requestHeaders
                      & canonicalizeHeaders
                      & unCanonicalized
                    , request & H.requestHeaders & joinHeaderNames
                    , uncompute payloadHash
                    ]

-- | Generate a 'StringToSign'
mkStringToSign :: SpacesRequest -> StringToSign
mkStringToSign req@SpacesRequest { .. } = StringToSign
    $ C.intercalate "\n"
                    [ "AWS4-HMAC-SHA256"
                    , fmtAmzTime time
                    , mkCredentials req & uncompute
                    , canonicalRequest & unCanonicalized & hashHex & uncompute
                    ]

-- | Generate a 'Signature'
mkSignature :: SpacesRequest -> StringToSign -> Signature
mkSignature SpacesRequest { .. } str = Signature
    . B16.encode
    . keyedHash (uncompute str)
    . keyedHash "aws4_request"
    . keyedHash "s3"
    . keyedHash (regionSlug region)
    . keyedHash (fmtAmzDate time)
    $ "AWS4" <> (secretKey & unSecretKey)
  where
    Spaces { .. } = spaces

-- | Create an 'Authorization' corresponding to the required AWS v4
-- @Authorization@ header
mkAuthorization :: SpacesRequest -> StringToSign -> Authorization
mkAuthorization req@SpacesRequest { .. } str = Authorization
    $ C.concat [ "AWS4-HMAC-SHA256 Credential="
                 <> (spaces & accessKey & unAccessKey)
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
                    , spaces & region & regionSlug
                    , "s3"
                    , "aws4_request"
                    ]

bodyBS :: MonadThrow m => RequestBody -> m ByteString
bodyBS (RequestBodyBS b)   = return b
bodyBS (RequestBodyLBS lb) = return $ LB.toStrict lb
bodyBS _                   =
    throwM $ InvalidRequest "Unsupported request body type"

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

hashHex :: ByteString -> Hashed
hashHex = Hashed . B16.encode . convert . hash @_ @SHA256
