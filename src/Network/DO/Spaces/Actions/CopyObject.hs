{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
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
                 ( Action(..)
                 , Bucket(Bucket)
                 , CannedACL
                 , ClientException(InvalidRequest)
                 , ETag
                 , Method(PUT)
                 , MonadSpaces
                 , Object(Object)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils
                 ( bshow
                 , etagP
                 , lastModifiedP
                 , showCannedACL
                 , xmlDocCursor
                 )

-- | Whether the 'Object''s metadata should be copied or replaced. Replace is
-- required to copy an object to itself
data MetadataDirective = Copy | Replace
    deriving ( Show, Eq, Generic )

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
    deriving ( Show, Eq, Generic )

data CopyObjectResponse =
    CopyObjectResponse { etag :: ETag, lastModified :: UTCTime }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m CopyObject where
    type SpacesResponse CopyObject = CopyObjectResponse

    buildRequest CopyObject { .. } = do
        when (and [ srcObject == destObject, metadataDirective == Copy ])
            . throwM
            . InvalidRequest
            $ mconcat [ "CopyObject: "
                      , "Object cannot be copied to itself unless "
                      , "REPLACE directive is specified"
                      ]
        spaces <- ask
        return SpacesRequestBuilder
               { object         = Just destObject
               , bucket         = Just destBucket
               , method         = Just PUT
               , body           = Nothing
               , queryString    = Nothing
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
