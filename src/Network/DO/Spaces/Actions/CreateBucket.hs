{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.CreateBucket
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.CreateBucket
    ( CreateBucket(..)
    , CreateBucketResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import qualified Data.CaseInsensitive    as CI
import           Data.Maybe              ( catMaybes )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils

-- | Create a new, empty 'Bucket'
data CreateBucket = CreateBucket
    { bucket :: Bucket -- ^ The name of the new 'Bucket' to create
    , region :: Maybe Region
    , acl    :: Maybe CannedACL
      -- ^ The 'CannedACL' to use; defaults to
      -- 'Network.DO.Spaces.Types.CannedACL.Private'
    }
    deriving stock ( Show, Eq, Generic )

type CreateBucketResponse = ()

instance MonadSpaces m => Action m CreateBucket where
    type ConsumedResponse CreateBucket = CreateBucketResponse

    buildRequest CreateBucket { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
             { bucket         = Just bucket
             , method         = Just PUT
             , overrideRegion = region
             , body           = Nothing
             , object         = Nothing
             , queryString    = Nothing
             , subresources   = Nothing
             , ..
             }
      where
        headers = catMaybes [ (CI.mk "x-amz-acl", ) . showCannedACL <$> acl ]

    consumeResponse _ = pure ()
