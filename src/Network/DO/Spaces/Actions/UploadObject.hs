{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.UploadObject
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.UploadObject
    ( UploadObject(..)
    , UploadObjectResponse(..)
    ) where

import           Control.Monad.Catch       ( MonadThrow(throwM) )
import           Control.Monad.Reader      ( MonadReader(ask) )
import           Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )

import qualified Data.CaseInsensitive      as CI

import           GHC.Generics              ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils
import           Network.HTTP.Conduit      ( RequestBody )
import           Network.Mime              ( MimeType )

-- | Upload a single object to Spaces. The maximum size for a single PUT request
-- is 5 GB
data UploadObject = UploadObject
    { bucket          :: Bucket
    , object          :: Object
    , body            :: RequestBody
    , optionalHeaders :: UploadHeaders
    , contentType     :: Maybe MimeType
    }
    deriving stock ( Generic )

data UploadObjectResponse = UploadObjectResponse
    { etag          :: ETag
    , contentLength :: Int -- ^ Length in bytes
    }
    deriving stock ( Show, Eq, Generic )

instance MonadSpaces m => Action m UploadObject where
    type ConsumedResponse UploadObject = UploadObjectResponse

    buildRequest UploadObject { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
             { bucket         = Just bucket
             , object         = Just object
             , method         = Just PUT
             , body           = Just body
             , queryString    = Nothing
             , subresources   = Nothing
             , overrideRegion = Nothing
             , ..
             }
      where
        headers = maybe id
                        (\ct -> (:) (CI.mk "Content-Type", ct))
                        contentType
                        (renderUploadHeaders optionalHeaders)

    consumeResponse raw = do
        resp <- runMaybeT
            $ UploadObjectResponse <$> (readEtag =<< lookupHeader' "etag")
            <*> (readContentLen =<< lookupHeader' "Content-Length")
        case resp of
            Just r  -> pure r
            Nothing -> throwM $ InvalidResponse "Missing/malformed headers"
      where
        lookupHeader' = lookupHeader raw
