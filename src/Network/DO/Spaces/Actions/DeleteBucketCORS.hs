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
-- Module      : Network.DO.Spaces.Actions.DeleteBucketCORS
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.DeleteBucketCORS
    ( DeleteBucketCORS(..)
    , DeleteBucketCORSResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(DELETE)
                 , MonadSpaces
                 , SpacesRequestBuilder(..)
                 )
import qualified Network.HTTP.Types      as H

-- | Delete all of a 'Bucket'\'s configured 'Network.DO.Spaces.Types.CORSRule's
data DeleteBucketCORS = DeleteBucketCORS { bucket :: Bucket }
    deriving ( Show, Eq, Generic )

type DeleteBucketCORSResponse = ()

instance MonadSpaces m => Action m DeleteBucketCORS where
    type (ConsumedResponse DeleteBucketCORS) = DeleteBucketCORSResponse

    buildRequest DeleteBucketCORS { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Just DELETE
               , body           = Nothing
               , object         = Nothing
               , overrideRegion = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , subresources   = Just
                     $ H.toQuery [ ( "cors" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }

    consumeResponse _ = return ()
