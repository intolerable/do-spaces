{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Network.DO.Spaces.Actions.GetObjectInfo
    ( GetObjectInfo(..)
    , GetObjectInfoResponse
    ) where

import           Control.Monad.Reader    ( MonadReader(ask) )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Method(HEAD)
                 , MonadSpaces
                 , Object
                 , ObjectMetadata(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( getObjectMetadata )

-- | Get information about an 'Object'; the response does not contain the
-- object itself
data GetObjectInfo = GetObjectInfo { bucket :: Bucket, object :: Object }
    deriving ( Show, Eq, Generic )

type GetObjectInfoResponse = ObjectMetadata

instance MonadSpaces m => Action m GetObjectInfo where
    type (SpacesResponse GetObjectInfo) = GetObjectInfoResponse

    buildRequest GetObjectInfo { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , object         = Just object
               , method         = Just HEAD
               , body           = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , overrideRegion = Nothing
               , ..
               }

    consumeResponse = getObjectMetadata
