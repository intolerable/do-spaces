{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.GetObjectACLs
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.GetObjectACLs
    ( GetObjectACLs(..)
    , GetObjectACLsResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( ACLResponse
                 , Action(..)
                 , Bucket
                 , MonadSpaces
                 , Object
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( aclP, xmlDocCursor )
import qualified Network.HTTP.Types      as H

-- | Get the full Access Control List associated with a 'Bucket'
data GetObjectACLs = GetObjectACLs { bucket :: Bucket, object :: Object }
    deriving ( Show, Eq, Generic )

type GetObjectACLsResponse = ACLResponse

instance MonadSpaces m => Action m GetObjectACLs where
    type ConsumedResponse GetObjectACLs = GetObjectACLsResponse

    buildRequest GetObjectACLs { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , object         = Just object
               , method         = Nothing
               , body           = Nothing
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
