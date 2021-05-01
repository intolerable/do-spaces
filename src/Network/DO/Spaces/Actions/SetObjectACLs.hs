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
-- Module      : Network.DO.Spaces.Actions.SetObjectACLs
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.SetObjectACLs
    ( SetObjectACLs(..)
    , SetObjectACLsResponse
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
                 , Object
                 , Owner
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils     ( writeACLSetter )
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import qualified Network.HTTP.Types          as H

-- | Get the full Access Control List associated with a 'Bucket'
data SetObjectACLs = SetObjectACLs
    { bucket :: Bucket
    , object :: Object
    , owner  :: Owner -- ^ The object owner
    , acls   :: [Grant]
    }
    deriving ( Show, Eq, Generic )

type SetObjectACLsResponse = ()

instance MonadSpaces m => Action m SetObjectACLs where
    type (ConsumedResponse SetObjectACLs) = SetObjectACLsResponse

    buildRequest soa@SetObjectACLs { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , object         = Just object
               , method         = Just PUT
               , body           = Just . RequestBodyLBS $ writeACLSetter soa
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

    consumeResponse _ = return ()
