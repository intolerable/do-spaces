{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- |
module Network.DO.Spaces.Actions.ListBucket
    ( ListBucket(..)
    , ListBucketResponse(..)
    ) where

import           GHC.Generics ( Generic )

data ListBucket = ListBucket
    deriving ( Show, Eq, Generic )

data ListBucketResponse = ListBucketResponse
    deriving ( Show, Eq, Generic )
