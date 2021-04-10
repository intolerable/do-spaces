{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Actions.CreateBucket
    ( CreateBucket(..)
    , CreateBucketResponse
    ) where

import qualified Data.CaseInsensitive    as CI

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , CannedACL
                 , Method(PUT)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( showCannedACL )

-- | Create a new, empty 'Bucket'
data CreateBucket = CreateBucket
    { bucket :: Bucket -- ^ The name of the new 'Bucket' to create
    , acl    :: Maybe CannedACL
      -- ^ The 'CannedACL' to use; defaults to 'Private'
    }
    deriving ( Show, Eq, Generic )

type CreateBucketResponse = ()

instance Action CreateBucket where
    type (SpacesResponse CreateBucket) = CreateBucketResponse

    buildRequest spaces CreateBucket { .. } = SpacesRequestBuilder
        { bucket      = Just bucket
        , method      = Just PUT
        , headers     = maybe mempty
                              (\a -> [ (CI.mk "x-amz-acl", showCannedACL a) ])
                              acl
        , body        = Nothing
        , object      = Nothing
        , queryString = Nothing
        , ..
        }

    consumeResponse _ = return ()
