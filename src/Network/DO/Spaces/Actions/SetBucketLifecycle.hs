{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.GetBucketLifecycle
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.SetBucketLifecycle
    ( SetBucketLifecycle(..)
    , SetBucketLifecycleResponse
    ) where

import           Control.Monad.Reader        ( MonadReader(ask) )

import           Data.ByteString             ( ByteString )
import           Data.Coerce                 ( coerce )
import qualified Data.Text                   as T
import           Data.Time.Format.ISO8601    ( iso8601Show )

import           GHC.Generics                ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , LifecycleExpiration(AfterDays, OnDate)
                 , LifecycleID(LifecycleID)
                 , LifecycleRule(..)
                 , Method(PUT)
                 , MonadSpaces
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils     ( mkNode, tshow )
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import qualified Network.HTTP.Types          as H

import qualified Text.XML                    as X

-- | Configure the 'LifecycleRule's for a 'Bucket'
data SetBucketLifecycle =
    SetBucketLifecycle { bucket :: Bucket, rules :: [LifecycleRule] }
    deriving ( Show, Eq, Generic )

type SetBucketLifecycleResponse = ()

instance MonadSpaces m => Action m SetBucketLifecycle where
    type ConsumedResponse SetBucketLifecycle = SetBucketLifecycleResponse

    buildRequest SetBucketLifecycle { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Just PUT
               , object         = Nothing
               , overrideRegion = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , subresources   = Just
                     $ H.toQuery [ ( "lifecycle" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }
      where
        body = Just . RequestBodyLBS . X.renderLBS X.def
            $ X.Document prologue root mempty

        prologue = X.Prologue mempty Nothing mempty

        root = X.Element name mempty (rulesNode <$> rules)
          where
            name = X.Name "LifecycleConfiguration"
                          (Just "http://s3.amazonaws.com/doc/2006-03-01/")
                          Nothing

        rulesNode LifecycleRule { .. } =
            X.NodeElement $ X.Element "Rule" mempty nodes
          where
            nodes = [ mkNode "ID" (coerce id')
                    , mkNode "Status" (showEnabled enabled)
                    ]
                <> foldMap (pure . mkNode "Prefix") prefix
                <> foldMap (pure . mkExpireNode) expiration
                <> foldMap (pure . mkAbortNode) abortIncomplete

            showEnabled = \case
                True  -> "Enabled"
                False -> "Disabled"

            mkExpireNode conf = X.NodeElement
                $ X.Element "Expiration" mempty (pure $ mkExNodes conf)
              where
                mkExNodes (AfterDays days) = mkNode "Days" (tshow days)
                mkExNodes (OnDate date)    =
                    mkNode "Date" (T.pack $ iso8601Show date)

            mkAbortNode days = X.NodeElement
                $ X.Element "AbortIncompleteMultipartUpload"
                            mempty
                            [ mkNode "DaysAfterInitiation" (tshow days) ]

    consumeResponse _ = return ()
