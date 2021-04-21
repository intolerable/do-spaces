{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.DeleteObject
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.DeleteObject
    ( DeleteObject(..)
    , DeleteObjectResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(DELETE)
                 , MonadSpaces
                 , Object
                 , SpacesRequestBuilder(..)
                 )

-- | Delete a single 'Object'
data DeleteObject = DeleteObject { bucket :: Bucket, object :: Object }
    deriving ( Show, Eq, Generic )

type DeleteObjectResponse = ()

instance MonadSpaces m => Action m DeleteObject where
    type (ConsumedResponse DeleteObject) = DeleteObjectResponse

    buildRequest DeleteObject { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , object         = Just object
               , method         = Just DELETE
               , body           = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , overrideRegion = Nothing
               , ..
               }

    consumeResponse _ = return ()
