{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.GetBucketLocation
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.GetBucketLocation
    ( GetBucketLocation(..)
    , GetBucketLocationResponse(..)
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )
import qualified Data.Text               as T

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils
import qualified Network.HTTP.Types      as H

import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($.//), (&/), (&|) )

-- | Query the location (the 'Region') of a 'Bucket'
newtype GetBucketLocation = GetBucketLocation
    { bucket :: Bucket
      -- ^ The name of the 'Bucket' whose location you'd like to retrieve
    }
    deriving stock ( Show, Generic )
    deriving newtype ( Eq )

newtype GetBucketLocationResponse = GetBucketLocationResponse
    { locationConstraint :: Region
      -- ^ The 'Region' of the queried 'Bucket'
    }
    deriving stock ( Show, Generic )
    deriving newtype ( Eq )

instance MonadSpaces m => Action m GetBucketLocation where
    type ConsumedResponse GetBucketLocation = GetBucketLocationResponse

    buildRequest GetBucketLocation { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Nothing
               , body           = Nothing
               , object         = Nothing
               , headers        = mempty
               , overrideRegion = Nothing
               , queryString    = Nothing
               , subresources   = Just
                     $ H.toQuery [ ( "location" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        GetBucketLocationResponse
            <$> (X.forceM (xmlElemError "LocationConstraint")
                 $ cursor $.// X.laxElement "LocationConstraint" &/ X.content
                 &| (slugToRegion . T.strip))
