{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Actions.ListAllBuckets
    ( ListAllBuckets(..)
    , ListAllBucketsResponse(..)
    ) where

import           Conduit                 ( (.|), MonadIO(liftIO), runConduit )

import           Data.Coerce             ( coerce )
import           Data.Sequence           ( Seq )
import qualified Data.Sequence           as S

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket(Bucket)
                 , BucketInfo(..)
                 , ID(..)
                 , Owner(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( xmlAttrError
                                         , xmlDatetimeP
                                         , xmlIntP
                                         )

import qualified Text.XML                as X
import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($/), (&/), (&|) )

data ListAllBuckets = ListAllBuckets
    deriving ( Show, Eq, Generic )

data ListAllBucketsResponse =
    ListAllBucketsResponse { owner :: Owner, buckets :: Seq BucketInfo }
    deriving ( Show, Eq, Generic )

instance Action ListAllBuckets where
    type SpacesResponse ListAllBuckets = ListAllBucketsResponse

    buildRequest spaces _ = SpacesRequestBuilder
        { body        = Nothing
        , method      = Nothing
        , object      = Nothing
        , queryString = Nothing
        , bucket      = Nothing
        , headers     = mempty
        , ..
        }

    consumeResponse raw = do
        cursor <- X.fromDocument
            <$> (liftIO . runConduit $ raw .| X.sinkDoc X.def)
        owner <- X.forceM (xmlAttrError "Owner")
            $ cursor $/ X.laxElement "Owner" &| ownerP
        buckets <- S.fromList
            <$> (sequence $ cursor $/ X.laxElement "Buckets" &| bucketsP)
        return ListAllBucketsResponse { .. }
      where
        ownerP c = do
            id' <- X.forceM (xmlAttrError "Owner ID")
                $ c $/ X.laxElement "ID" &/ X.content &| xmlIntP @_ @ID
            return Owner { displayName = id', id' }

        bucketsP c = X.forceM (xmlAttrError "Bucket")
            $ c $/ X.laxElement "Bucket" &| bucketInfoP

        bucketInfoP c = do
            name <- coerce
                <$> (X.force (xmlAttrError "Name")
                     $ c $/ X.laxElement "Name" &/ X.content)
            creationDate <- X.forceM (xmlAttrError "Creation date")
                $ c $/ X.laxElement "CreationDate" &/ X.content
                &| xmlDatetimeP
            return BucketInfo { .. }
