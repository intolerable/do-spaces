{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.SetBucketCORS
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.SetBucketCORS
    ( SetBucketCORSResponse
    , SetBucketCORS(..)
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )
import qualified Data.CaseInsensitive    as CI
import qualified Data.Text.Encoding      as T

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils
import           Network.HTTP.Conduit    ( RequestBody(RequestBodyLBS) )
import qualified Network.HTTP.Types      as H

import qualified Text.XML                as X

-- | Set a 'Bucket'\'s 'CORSRule's
data SetBucketCORS = SetBucketCORS { bucket :: Bucket, rules :: [CORSRule] }
    deriving stock ( Show, Eq, Generic )

type SetBucketCORSResponse = ()

instance MonadSpaces m => Action m SetBucketCORS where
    type ConsumedResponse SetBucketCORS = SetBucketCORSResponse

    buildRequest SetBucketCORS { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
             { bucket         = Just bucket
             , method         = Just PUT
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
      where
        body = Just . RequestBodyLBS . X.renderLBS X.def
            $ X.Document prologue root mempty

        prologue = X.Prologue mempty Nothing mempty

        root = X.Element "CORSConfiguration" mempty (ruleNode <$> rules)

        ruleNode CORSRule { .. } = X.NodeElement . X.Element "CORSRule" mempty
            $ mconcat [ [ mkNode "AllowedOrigin" allowedOrigin ]
                      , mkNode "AllowedHeader" . T.decodeUtf8 . CI.original
                        <$> allowedHeaders
                      , mkNode "AllowedMethod" . tshow <$> allowedMethods
                      ]

    consumeResponse _ = pure ()
