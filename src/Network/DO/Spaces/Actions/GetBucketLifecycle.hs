{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Network.DO.Spaces.Actions.GetBucketLifecycle
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.GetBucketLifecycle
    ( GetBucketLifecycle(..)
    , GetBucketLifecycleResponse(..)
    ) where

import           Control.Monad           ( join )
import           Control.Monad.Catch     ( MonadThrow(throwM) )
import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )
import           Data.Coerce             ( coerce )
import           Data.Maybe              ( listToMaybe )
import qualified Data.Text               as T

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils
import qualified Network.HTTP.Types      as H

import           Text.Read               ( readMaybe )
import qualified Text.XML                as X
import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($/), (&/), (&|) )
import           Text.XML.Cursor.Generic ( Cursor )

-- | Get the 'LifecycleRule' configuration for a 'Bucket'. Note that unless
-- you have explicitly configured lifecycle rules, this will fail with a 404
-- status and an error code of @NoSuchLifecycleConfiguration@
newtype GetBucketLifecycle = GetBucketLifecycle { bucket :: Bucket }
    deriving stock ( Show, Generic )
    deriving newtype ( Eq )

newtype GetBucketLifecycleResponse =
    GetBucketLifecycleResponse { rules :: [LifecycleRule] }
    deriving stock ( Show, Generic )
    deriving newtype ( Eq )

instance MonadSpaces m => Action m GetBucketLifecycle where
    type ConsumedResponse GetBucketLifecycle = GetBucketLifecycleResponse

    buildRequest GetBucketLifecycle { .. } = do
        spaces <- ask
        pure SpacesRequestBuilder
             { bucket         = Just bucket
             , method         = Nothing
             , body           = Nothing
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

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        GetBucketLifecycleResponse
            <$> sequence (cursor $/ X.laxElement "Rule" &| ruleP)

ruleP :: MonadThrow m => Cursor X.Node -> m LifecycleRule
ruleP c = do
    lifecycleID <- X.force (xmlElemError "ID")
        $ c $/ X.laxElement "ID" &/ X.content &| coerce
    enabled <- X.forceM (xmlElemError "Status")
        $ c $/ X.laxElement "Status" &/ X.content &| readStatus
    pure LifecycleRule { .. }
  where
    prefix = xmlMaybeElem c "Prefix"

    abortIncomplete = join . listToMaybe
        $ c $/ X.laxElement "AbortIncompleteMultipartUpload" &| abortP

    expiration = join . sequence . listToMaybe
        $ c $/ X.laxElement "Expiration" &| expiresP

    abortP c' = X.forceM (xmlElemError "DaysAfterInitiation")
        $ c' $/ X.laxElement "DaysAfterInitiation" &/ X.content
        &| (readMaybe . T.unpack)

    readStatus = \case
        "Enabled"  -> pure True
        "Disabled" -> pure False
        _          -> throwM $ InvalidXML "GetBucketLifecycle: invalid Status"

    -- TODO find a less hideous way of doing this
    expiresP (X.node -> X.NodeElement (X.Element _ _ elems)) = case elems of
        (_ : el : _) -> case el of
            X.NodeElement (X.Element (X.nameLocalName -> name) _ _)
                | name == "Days" -> AfterDays
                    <$> (readMaybe . T.unpack . mconcat
                         $ (X.fromNode el $/ X.content))
                | name == "Date" -> OnDate
                    <$> (xmlUTCTime @Maybe . mconcat
                         $ (X.fromNode el $/ X.content))
            _ -> throwInvalidExpires
        _            -> throwInvalidExpires

    expiresP _ = throwInvalidExpires

    throwInvalidExpires =
        throwM $ InvalidXML "GetBucketLifecycle: invalid Expiration"
