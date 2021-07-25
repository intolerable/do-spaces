{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.GetBucketLifecycle
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.DeleteBucketLifecycle
    ( DeleteBucketLifecycle(..)
    , DeleteBucketLifecycleResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import qualified Network.HTTP.Types      as H

newtype DeleteBucketLifecycle = DeleteBucketLifecycle { bucket :: Bucket }
    deriving stock ( Show, Generic )
    deriving newtype ( Eq )

type DeleteBucketLifecycleResponse = ()

instance MonadSpaces m => Action m DeleteBucketLifecycle where
    type ConsumedResponse DeleteBucketLifecycle = DeleteBucketLifecycleResponse

    buildRequest DeleteBucketLifecycle { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
             { bucket         = Just bucket
             , method         = Just DELETE
             , body           = Nothing
             , object         = Nothing
             , overrideRegion = Nothing
             , queryString    = Nothing
             , headers        = mempty
             , subresources   = Just
                   $ H.toQuery [ ( "lifecycle" :: ByteString
                                 , Nothing :: Maybe ByteString
                                 )
                               ]
             , ..
             }

    consumeResponse _ = pure ()
