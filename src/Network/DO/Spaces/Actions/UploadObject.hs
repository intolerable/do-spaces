{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
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

import           GHC.Generics              ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , ClientException(OtherError)
                 , ETag
                 , Method(PUT)
                 , MonadSpaces
                 , Object
                 , SpacesRequestBuilder(..)
                 , UploadHeaders(..)
                 )
import           Network.DO.Spaces.Utils
                 ( lookupHeader
                 , readContentLen
                 , readEtag
                 , renderUploadHeaders
                 )
import           Network.HTTP.Conduit      ( RequestBody )

-- | Upload a single object to Spaces. The maximum size for a single PUT request
-- is 5 GB
data UploadObject = UploadObject
    { object          :: Object
    , bucket          :: Bucket
    , body            :: RequestBody
    , optionalHeaders :: UploadHeaders
    }
    deriving ( Generic )

data UploadObjectResponse = UploadObjectResponse
    { etag          :: ETag
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
               , headers        = renderUploadHeaders optionalHeaders
               , ..
               }

    consumeResponse raw = do
        resp <- runMaybeT
            $ UploadObjectResponse <$> (readEtag =<< lookupHeader' "etag")
            <*> (readContentLen =<< lookupHeader' "Content-Length")
        case resp of
            Just r  -> return r
            Nothing -> throwM $ OtherError "Missing/malformed headers"
      where
        lookupHeader' = lookupHeader raw
