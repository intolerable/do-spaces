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
-- Module      : Network.DO.Spaces.Actions.GetBucketCORS
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.GetBucketCORS
    ( GetBucketCORS(..)
    , GetBucketCORSResponse(..)
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )
import qualified Data.CaseInsensitive    as CI
import           Data.Maybe              ( mapMaybe )
import           Data.Sequence           ( Seq )
import qualified Data.Sequence           as S
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , CORSRule(..)
                 , MonadSpaces
                 , SpacesRequestBuilder(..)
                 , mkCORSRule
                 )
import           Network.DO.Spaces.Utils ( xmlDocCursor, xmlElemError )
import qualified Network.HTTP.Types      as H

import           Text.Read               ( readMaybe )
import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($/), (&/), (&|) )

-- | Get the 'CORSRule's associated with a 'Bucket'
data GetBucketCORS = GetBucketCORS { bucket :: Bucket }
    deriving ( Show, Eq, Generic )

data GetBucketCORSResponse = GetBucketCORSResponse { rules :: Seq CORSRule }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m GetBucketCORS where
    type (ConsumedResponse GetBucketCORS) = GetBucketCORSResponse

    buildRequest GetBucketCORS { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Nothing
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

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        GetBucketCORSResponse . S.fromList
            <$> sequence (cursor $/ X.laxElement "CORSRule" &| ruleP)
      where
        ruleP c = do
            allowedOrigin <- X.force (xmlElemError "AllowedOrigin")
                $ c $/ X.laxElement "AllowedOrigin" &/ X.content

            mkCORSRule allowedOrigin
                       (mapMaybe (readMaybe . T.unpack) allowedMethods)
                       (CI.mk . T.encodeUtf8 <$> allowedHeaders)
          where
            allowedHeaders = c $/ X.laxElement "AllowedHeader" &/ X.content

            allowedMethods = c $/ X.laxElement "AllowedMethod" &/ X.content
