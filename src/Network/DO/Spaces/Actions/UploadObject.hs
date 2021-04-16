{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Network.DO.Spaces.Actions.UploadObject
    ( UploadObject(..)
    , UploadObjectResponse(..)
    ) where

import           Control.Monad.Catch       ( MonadThrow(throwM) )
import           Control.Monad.Reader      ( MonadReader(ask) )
import           Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )

import           Data.Bifunctor            ( Bifunctor(second, first) )
import qualified Data.CaseInsensitive      as CI
import           Data.Maybe                ( catMaybes )
import           Data.Text                 ( Text )
import qualified Data.Text.Encoding        as T

import           GHC.Generics              ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , CannedACL
                 , ClientException(OtherError)
                 , Method(PUT)
                 , MonadSpaces
                 , Object
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils
                 ( lookupHeader
                 , readContentLen
                 , readEtag
                 , showCannedACL
                 )
import           Network.HTTP.Conduit      ( RequestBody )

-- | Upload a single object to Spaces. The maximum size for a single PUT request
-- is 5 GB
data UploadObject = UploadObject
    { object             :: Object
    , bucket             :: Bucket
    , body               :: RequestBody
    , acl                :: Maybe CannedACL
    , cacheControl       :: Maybe Text
    , contentDisposition :: Maybe Text
    , contentEncoding    :: Maybe Text
    , metadata           :: [(Text, Text)]
      -- ^ Arbitrary key-value pairs supplied by the user. Each pair expands into
      -- @x-amz-meta-*@, e.g. @x-amz-meta-s3cmd-attrs: uid:1000/gname:asb...@
    }
    deriving ( Generic )

data UploadObjectResponse = UploadObjectResponse
    { etag          :: Text -- ^ MD5 hash of the object
    , contentLength :: Int -- ^ Length in bytes
    }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m UploadObject where
    type (SpacesResponse UploadObject) = UploadObjectResponse

    buildRequest UploadObject { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , object         = Just object
               , method         = Just PUT
               , body           = Just body
               , queryString    = Nothing
               , overrideRegion = Nothing
               , ..
               }
      where
        headers = second T.encodeUtf8
            <$> catMaybes [ ("x-amz-acl", ) . showCannedACL <$> acl
                          , ("Cache-Control", ) <$> cacheControl
                          , ("Content-Disposition", ) <$> contentDisposition
                          , ("Content-Encoding", ) <$> contentEncoding
                          ]
            <> (first (CI.mk . T.encodeUtf8 . ("x-amz-meta-" <>)) <$> metadata)

    consumeResponse raw = do
        resp <- runMaybeT
            $ UploadObjectResponse <$> (readEtag =<< lookupHeader' "etag")
            <*> (readContentLen =<< lookupHeader' "Content-Length")
        case resp of
            Just r  -> return r
            Nothing -> throwM $ OtherError "Missing/malformed headers"
      where
        lookupHeader' = lookupHeader raw
