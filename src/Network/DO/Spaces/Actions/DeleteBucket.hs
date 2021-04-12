{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Network.DO.Spaces.Actions.DeleteBucket
    ( DeleteBucket(..)
    , DeleteBucketResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(DELETE)
                 , MonadSpaces
                 , SpacesRequestBuilder(..)
                 )

-- | Delete a single 'Bucket'. Note that it must be empty
data DeleteBucket = DeleteBucket
    { bucket :: Bucket -- ^ The name of the 'Bucket' to delete
    }
    deriving ( Show, Eq, Generic )

type DeleteBucketResponse = ()

instance MonadSpaces m => Action m DeleteBucket where
    type (SpacesResponse DeleteBucket) = DeleteBucketResponse

    buildRequest DeleteBucket { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { method      = Just DELETE
               , bucket      = Just bucket
               , body        = Nothing
               , object      = Nothing
               , queryString = Nothing
               , headers     = mempty
               , ..
               }

    consumeResponse _ = return ()
