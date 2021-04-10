{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Actions.DeleteBucket
    ( DeleteBucket(..)
    , DeleteBucketResponse
    ) where

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(DELETE)
                 , SpacesRequestBuilder(..)
                 )

-- | Delete a single 'Bucket'. Note that it must be empty
data DeleteBucket = DeleteBucket
    { bucket :: Bucket -- ^ The name of the 'Bucket' to delete
    }
    deriving ( Show, Eq, Generic )

type DeleteBucketResponse = ()

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

    consumeResponse _ = return ()
