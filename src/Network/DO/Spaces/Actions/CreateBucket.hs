{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Network.DO.Spaces.Actions.CreateBucket
    ( CreateBucket(..)
    , CreateBucketResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import qualified Data.CaseInsensitive    as CI
import           Data.Maybe              ( catMaybes )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , CannedACL
                 , Method(PUT)
                 , MonadSpaces
                 , Region
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( showCannedACL )

-- | Create a new, empty 'Bucket'
data CreateBucket = CreateBucket
    { bucket :: Bucket -- ^ The name of the new 'Bucket' to create
    , region :: Maybe Region
    , acl    :: Maybe CannedACL
      -- ^ The 'CannedACL' to use; defaults to 'Private'
    }
    deriving ( Show, Eq, Generic )

type CreateBucketResponse = ()

instance MonadSpaces m => Action m CreateBucket where
    type (SpacesResponse CreateBucket) = CreateBucketResponse

    buildRequest CreateBucket { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Just PUT
               , overrideRegion = region
               , body           = Nothing
               , object         = Nothing
               , queryString    = Nothing
               , ..
               }
      where
        headers = catMaybes [ (CI.mk "x-amz-acl", ) . showCannedACL <$> acl ]

    consumeResponse _ = return ()
