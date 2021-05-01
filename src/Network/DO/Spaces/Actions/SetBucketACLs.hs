{-# LANGUAGE DataKinds #-}
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
-- Module      : Network.DO.Spaces.Actions.SetBucketACLs
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.SetBucketACLs
    ( SetBucketACLs(..)
    , SetBucketACLsResponse
    ) where

import           Control.Monad.Reader        ( MonadReader(ask) )

import           Data.ByteString             ( ByteString )

import           GHC.Generics                ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Grant(..)
                 , Method(PUT)
                 , MonadSpaces
                 , Owner
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils     ( writeACLSetter )
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import qualified Network.HTTP.Types          as H

data SetBucketACLs = SetBucketACLs
    { bucket :: Bucket
    , acls   :: [Grant]
    , owner  :: Owner -- ^ The bucket owner
    }
    deriving ( Show, Eq, Generic )

type SetBucketACLsResponse = ()

instance MonadSpaces m => Action m SetBucketACLs where
    type ConsumedResponse SetBucketACLs = SetBucketACLsResponse

    buildRequest sba@SetBucketACLs { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Just PUT
               , object         = Nothing
               , overrideRegion = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , body           = Just . RequestBodyLBS $ writeACLSetter sba
               , subresources   = Just
                     $ H.toQuery [ ( "acl" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }

    consumeResponse _ = return ()
