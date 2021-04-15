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
module Network.DO.Spaces.Actions.ListBucket
    ( ListBucket(..)
    , ListBucketResponse(..)
    ) where

import           Control.Monad           ( when )
import           Control.Monad.Catch     ( MonadThrow(throwM) )
import           Control.Monad.Extra     ( orM )
import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.Bool               ( bool )
import           Data.ByteString         ( ByteString )
import qualified Data.ByteString.Char8   as C
import           Data.Coerce             ( coerce )
import           Data.Sequence           ( Seq )
import qualified Data.Sequence           as S
import           Data.Text               ( Text )
import qualified Data.Text.Encoding      as T

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket(Bucket)
                 , ClientException(InvalidRequest)
                 , MonadSpaces
                 , Object(..)
                 , ObjectInfo(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils
                 ( bshow
                 , etagP
                 , lastModifiedP
                 , ownerP
                 , xmlAttrError
                 , xmlDocCursor
                 , xmlIntP
                 , xmlMaybeAttr
                 )
import qualified Network.HTTP.Types      as H

import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($/), (&/), (&|) )

-- | List the contents ('Object's) of a 'Bucket'
data ListBucket = ListBucket
    { bucket    :: Bucket
    , delimiter :: Maybe Char -- ^ Character used to group keys
    , marker    :: Maybe Object
      -- ^ The 'Object' to start with when listing the bucket's contents
    , maxKeys   :: Maybe Int
      -- ^ Max number of 'Object's to return, between 0 and 1,000 (inclusive)
    , prefix    :: Maybe Text
      -- ^ String value to group keys. Only objects whose names begin with the
      -- prefix are returned
    }
    deriving ( Show, Eq, Generic )

data ListBucketResponse = ListBucketResponse
    { bucket      :: Bucket -- ^ The 'Bucket' name
    , prefix      :: Maybe Text -- ^ The 'Object' prefix, if supplied as a query param
    , marker      :: Maybe Object
      -- ^ An 'Object' indicating where the list of 'Object's begin; 'Nothing'
      -- denotes the beginning of the list
    , nextMarker  :: Maybe Object
      -- ^ The 'Object' that should be used as the 'marker' query param in
      -- subsequent requests
    , maxKeys     :: Int
      -- ^ Maximum number of 'ObjectInfo's to include; based on request parameter
      -- of the same name
    , isTruncated :: Bool
      -- ^ Indicates whether the response contains all possible 'Object's
    , objects     :: Seq ObjectInfo
    }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m ListBucket where
    type (SpacesResponse ListBucket) = ListBucketResponse

    buildRequest ListBucket { .. } = do
        when (Just True == orM [ (< 0) <$> maxKeys, (> 1000) <$> maxKeys ])
            . throwM
            $ InvalidRequest "ListBucket: maxKeys must be >= 0 && <= 1000"
        spaces <- ask

        return SpacesRequestBuilder
               { bucket         = Just bucket
               , body           = Nothing
               , object         = Nothing
               , method         = Nothing
               , headers        = mempty
               , overrideRegion = Nothing
               , ..
               }
      where
        queryString = Just
            $ H.toQuery [ ("delimiter" :: ByteString, ) . C.singleton
                          <$> delimiter
                        , ("marker", ) . T.encodeUtf8 . unObject <$> marker
                        , ("max-keys", ) . bshow <$> maxKeys
                        , ("prefix", ) . T.encodeUtf8 <$> prefix
                        ]

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        bucket <- X.force (xmlAttrError "Name")
            $ cursor $/ X.laxElement "Name" &/ X.content &| coerce
        maxKeys <- X.forceM (xmlAttrError "MaxKeys")
            $ cursor $/ X.laxElement "MaxKeys" &/ X.content &| xmlIntP
        isTruncated <- X.force (xmlAttrError "IsTruncated")
            $ cursor $/ X.laxElement "IsTruncated" &/ X.content &| truncP
        objects <- S.fromList
            <$> (sequence
                 $ cursor $/ X.laxElement "Contents" &| objectMetadataP)
        let prefix     = xmlMaybeAttr cursor "Prefix"
            marker     = coerce <$> xmlMaybeAttr cursor "Marker"
            nextMarker = coerce <$> xmlMaybeAttr cursor "NextMarker"
        return ListBucketResponse { .. }
      where
        truncP t = bool False True (t == "true")

        objectMetadataP c = do
            object <- X.force (xmlAttrError "Key")
                $ c $/ X.laxElement "Key" &/ X.content &| coerce
            lastModified <- lastModifiedP c
            etag <- etagP c
            size <- X.forceM (xmlAttrError "Size")
                $ c $/ X.laxElement "Size" &/ X.content &| xmlIntP
            owner <- X.forceM (xmlAttrError "Owner")
                $ c $/ X.laxElement "Owner" &| ownerP
            return ObjectInfo { .. }
