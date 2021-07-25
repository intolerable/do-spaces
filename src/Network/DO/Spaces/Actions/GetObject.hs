{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.GetObject
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.GetObject
    ( GetObject(..)
    , GetObjectResponse(..)
    ) where

import           Conduit                 ( (.|), runConduit )

import           Control.Monad.Reader    ( MonadReader(ask) )

import qualified Data.ByteString.Lazy    as LB
import           Data.Conduit.Binary     ( sinkLbs )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils

-- | Retrieve an 'Object' along with its associated metadata. The object's data
-- is read into a lazy 'LB.ByteString'
data GetObject = GetObject { bucket :: Bucket, object :: Object }
    deriving stock ( Show, Eq, Generic )

data GetObjectResponse = GetObjectResponse
    { objectMetadata :: ObjectMetadata, objectData :: LB.ByteString }
    deriving stock ( Show, Eq, Generic )

instance MonadSpaces m => Action m GetObject where
    type ConsumedResponse GetObject = GetObjectResponse

    buildRequest GetObject { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
             { bucket         = Just bucket
             , object         = Just object
             , method         = Nothing
             , body           = Nothing
             , queryString    = Nothing
             , subresources   = Nothing
             , overrideRegion = Nothing
             , headers        = mempty
             , ..
             }

    consumeResponse raw@RawResponse { .. } = GetObjectResponse
        <$> lookupObjectMetadata raw
        <*> runConduit (body .| sinkLbs)
