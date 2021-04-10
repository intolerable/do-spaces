{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Actions.DeleteBucket
    ( DeleteBucket(..)
    , DeleteBucketResponse(..)
    ) where

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(DELETE)
                 , SpacesRequestBuilder(..)
                 )

data DeleteBucket = DeleteBucket { bucket :: Bucket }
    deriving ( Show, Eq, Generic )

data DeleteBucketResponse = DeleteBucketResponse
    deriving ( Show, Eq, Generic )

instance Action DeleteBucket where
    type (SpacesResponse DeleteBucket) = DeleteBucketResponse

    buildRequest spaces DeleteBucket { .. } = SpacesRequestBuilder
        { method      = Just DELETE
        , bucket      = Just bucket
        , body        = Nothing
        , object      = Nothing
        , queryString = Nothing
        , headers     = mempty
        , ..
        }

    consumeResponse _ = return DeleteBucketResponse
