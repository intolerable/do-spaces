{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Actions.ListBucket
    ( ListBucket(..)
    , ListBucketResponse(..)
    ) where

import           Data.Bool               ( bool )
import           Data.ByteString         ( ByteString )
import qualified Data.ByteString.Char8   as C
import           Data.Coerce             ( coerce )
import           Data.Sequence           ( Seq )
import qualified Data.Sequence           as S
import           Data.Text               ( Text )
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket(Bucket)
                 , Object(..)
                 , ObjectInfo(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils
                 ( bshow
                 , ownerP
                 , xmlAttrError
                 , xmlDatetimeP
                 , xmlDocCursor
                 , xmlIntP
                 , xmlMaybeField
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
    , maxKeys   :: Maybe Int -- ^ Max number of 'Object's to return (inclusive)
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
      -- ^ Indicates whether the response contains all 'Object's
    , objects     :: Seq ObjectInfo
    }
    deriving ( Show, Eq, Generic )

instance Action ListBucket where
    type (SpacesResponse ListBucket) = ListBucketResponse

    buildRequest spaces ListBucket { .. } = SpacesRequestBuilder
        { bucket      = Just bucket
        , body        = Nothing
        , object      = Nothing
        , method      = Nothing
        , headers     = mempty
        , queryString = Just
              $ H.toQuery [ ("delimiter" :: ByteString, ) . C.singleton
                            <$> delimiter
                          , ("marker", ) . T.encodeUtf8 . unObject <$> marker
                          , ("max-keys", ) . bshow <$> maxKeys
                          , ("prefix", ) . T.encodeUtf8 <$> prefix
                          ]
        , ..
        }

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        bucket <- X.force (xmlAttrError "Name")
            $ cursor $/ X.laxElement "Name" &/ X.content &| coerce
        maxKeys <- X.forceM (xmlAttrError "MaxKeys")
            $ cursor $/ X.laxElement "MaxKeys" &/ X.content &| xmlIntP
        isTruncated <- X.force (xmlAttrError "IsTruncated")
            $ cursor $/ X.laxElement "IsTruncated" &/ X.content &| truncP
        -- owner <- X.forceM (xmlAttrError "Owner") $ cursor $/ X.laxElement "Owner" &| ownerP
        objects <- S.fromList
            <$> (sequence $ cursor $/ X.laxElement "Contents" &| objectInfoP)
        let prefix     = xmlMaybeField cursor "Prefix"
            marker     = coerce <$> xmlMaybeField cursor "Marker"
            nextMarker = coerce <$> xmlMaybeField cursor "NextMarker"
        return ListBucketResponse { .. }
      where
        truncP t = bool False True (t == "true")

        objectInfoP c = do
            object <- X.force (xmlAttrError "Key")
                $ c $/ X.laxElement "Key" &/ X.content &| coerce
            lastModified <- X.forceM (xmlAttrError "LastModified")
                $ c $/ X.laxElement "LastModified" &/ X.content
                &| xmlDatetimeP
            etag <- X.force (xmlAttrError "ETag")
                $ c $/ X.laxElement "ETag" &/ X.content
                &| T.dropAround ('"' ==)
            size <- X.forceM (xmlAttrError "Size")
                $ c $/ X.laxElement "Size" &/ X.content &| xmlIntP
            owner <- X.forceM (xmlAttrError "Owner")
                $ c $/ X.laxElement "Owner" &| ownerP
            return ObjectInfo { .. }
