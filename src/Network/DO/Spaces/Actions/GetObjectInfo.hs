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
    , ObjectInfoResponse
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
import           Network.DO.Spaces.Utils ( objectMetadataP )

-- | Get information about an 'Object'; the response does not contain the
-- object itself
data GetObjectInfo = GetObjectInfo { object :: Object, bucket :: Bucket }
    deriving ( Show, Eq, Generic )

type ObjectInfoResponse = ObjectMetadata

instance MonadSpaces m => Action m GetObjectInfo where
    type (SpacesResponse GetObjectInfo) = ObjectInfoResponse

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

    consumeResponse = objectMetadataP
