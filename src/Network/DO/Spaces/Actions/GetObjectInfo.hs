{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.GetObjectInfo
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.GetObjectInfo
    ( GetObjectInfo(..)
    , GetObjectInfoResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(HEAD)
                 , MonadSpaces
                 , Object
                 , ObjectMetadata(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( lookupObjectMetadata )

-- | Get information about an 'Object'; the response does not contain the
-- object itself
data GetObjectInfo = GetObjectInfo { bucket :: Bucket, object :: Object }
    deriving ( Show, Eq, Generic )

type GetObjectInfoResponse = ObjectMetadata

instance MonadSpaces m => Action m GetObjectInfo where
    type (ConsumedResponse GetObjectInfo) = GetObjectInfoResponse

    buildRequest GetObjectInfo { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , object         = Just object
               , method         = Just HEAD
               , body           = Nothing
               , queryString    = Nothing
               , subresources   = Nothing
               , headers        = mempty
               , overrideRegion = Nothing
               , ..
               }

    consumeResponse = lookupObjectMetadata
