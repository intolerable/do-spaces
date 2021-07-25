{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Actions.CopyObject
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Network.DO.Spaces.Actions.CopyObject
    ( MetadataDirective(..)
    , CopyObject(..)
    , CopyObjectResponse(..)
    ) where

import           Control.Monad           ( when )
import           Control.Monad.Catch     ( MonadThrow(throwM) )
import           Control.Monad.Reader    ( MonadReader(ask) )

import qualified Data.ByteString.Char8   as C
import qualified Data.CaseInsensitive    as CI
import           Data.Char               ( toUpper )
import           Data.Coerce             ( coerce )
import           Data.Maybe              ( catMaybes )
import qualified Data.Text.Encoding      as T
import           Data.Time               ( UTCTime )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils

-- | Whether the 'Object'\'s metadata should be copied or replaced. Replace is
-- required to copy an object to itself
data MetadataDirective
    = Copy
    | Replace
    deriving stock ( Show, Eq, Generic )

-- | Copy and 'Object' from one 'Bucket' to another. Both buckets must
-- be in the same region
data CopyObject = CopyObject
    { srcBucket         :: Bucket
    , destBucket        :: Bucket
    , srcObject         :: Object
    , destObject        :: Object
    , metadataDirective :: MetadataDirective
    , acl               :: Maybe CannedACL
    }
    deriving stock ( Show, Eq, Generic )

data CopyObjectResponse =
    CopyObjectResponse { etag :: ETag, lastModified :: UTCTime }
    deriving stock ( Show, Eq, Generic )

instance MonadSpaces m => Action m CopyObject where
    type ConsumedResponse CopyObject = CopyObjectResponse

    buildRequest CopyObject { .. } = do
        when (and [ srcObject == destObject, metadataDirective == Copy ])
            . throwM
            . InvalidRequest
            $ mconcat [ "CopyObject: "
                      , "Object cannot be copied to itself unless "
                      , "REPLACE directive is specified"
                      ]
        spaces <- ask
        pure SpacesRequestBuilder
             { object         = Just destObject
             , bucket         = Just destBucket
             , method         = Just PUT
             , body           = Nothing
             , queryString    = Nothing
             , subresources   = Nothing
             , overrideRegion = Nothing
             , ..
             }
      where
        headers = [ ( CI.mk "x-amz-copy-source"
                    , mconcat [ "/"
                              , T.encodeUtf8 $ coerce srcBucket
                              , "/"
                              , T.encodeUtf8 $ coerce srcObject
                              ]
                    )
                  , ( CI.mk "x-amz-metadata-directive"
                    , C.map toUpper $ bshow metadataDirective
                    )
                  ]
            <> catMaybes [ (CI.mk "x-amz-acl", ) . showCannedACL <$> acl ]

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        CopyObjectResponse <$> etagP cursor <*> lastModifiedP cursor
