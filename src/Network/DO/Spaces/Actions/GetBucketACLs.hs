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
-- Module      : Network.DO.Spaces.Actions.GetBucketACLs
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.GetBucketACLs
    ( GetBucketACLs(..)
    , GetBucketACLsResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils
import qualified Network.HTTP.Types      as H

-- | Get the full Access Control List associated with a 'Bucket'
newtype GetBucketACLs = GetBucketACLs { bucket :: Bucket }
    deriving stock ( Show, Generic )
    deriving newtype ( Eq )

type GetBucketACLsResponse = ACLResponse

instance MonadSpaces m => Action m GetBucketACLs where
    type ConsumedResponse GetBucketACLs = GetBucketACLsResponse

    buildRequest GetBucketACLs { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Nothing
               , body           = Nothing
               , object         = Nothing
               , overrideRegion = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , subresources   = Just
                     $ H.toQuery [ ( "acl" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }

    consumeResponse raw = aclP =<< xmlDocCursor raw
