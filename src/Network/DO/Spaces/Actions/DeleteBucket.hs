{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.DeleteBucket
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.DeleteBucket
    ( DeleteBucket(..)
    , DeleteBucketResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(DELETE)
                 , MonadSpaces
                 , SpacesRequestBuilder(..)
                 )

-- | Delete a single 'Bucket'. Note that it must be empty
data DeleteBucket = DeleteBucket
    { bucket :: Bucket -- ^ The name of the 'Bucket' to delete
    }
    deriving ( Show, Eq, Generic )

type DeleteBucketResponse = ()

instance MonadSpaces m => Action m DeleteBucket where
    type (ConsumedResponse DeleteBucket) = DeleteBucketResponse

    buildRequest DeleteBucket { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { method         = Just DELETE
               , bucket         = Just bucket
               , body           = Nothing
               , object         = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , overrideRegion = Nothing
               , ..
               }

    consumeResponse _ = return ()
