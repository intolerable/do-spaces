{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Network.DO.Spaces.Actions.GetBucketLocation
    ( GetBucketLocation(..)
    , GetBucketLocationResponse(..)
    ) where

import           Control.Monad.Catch     ( MonadThrow(throwM) )
import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )
import qualified Data.Text               as T

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , ClientException(InvalidXML)
                 , MonadSpaces
                 , Region(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( quote, xmlAttrError, xmlDocCursor )
import qualified Network.HTTP.Types      as H

import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($.//), (&/), (&|) )

-- | Query the location (the 'Region') of a 'Bucket'
data GetBucketLocation = GetBucketLocation
    { bucket :: Bucket
      -- ^ The name of the 'Bucket' whose location you'd like to retrieve
    }
    deriving ( Show, Eq, Generic )

data GetBucketLocationResponse = GetBucketLocationResponse
    { locationConstraint :: Region
      -- ^ The 'Region' of the queried 'Bucket'
    }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m GetBucketLocation where
    type (SpacesResponse GetBucketLocation) = GetBucketLocationResponse

    buildRequest GetBucketLocation { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Nothing
               , body           = Nothing
               , object         = Nothing
               , headers        = mempty
               , overrideRegion = Nothing
               , queryString    = Just
                     $ H.toQuery [ ( "location" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        GetBucketLocationResponse
            <$> (X.forceM (xmlAttrError "LocationConstraint")
                 $ cursor $.// X.laxElement "LocationConstraint" &/ X.content
                 &| slugToRegion . T.strip)
      where
        slugToRegion = \case
            "nyc3" -> return NewYork
            "ams3" -> return Amsterdam
            "sfo3" -> return SanFrancisco
            "sgp1" -> return Singapore
            "fra1" -> return Frankfurt
            reg    -> throwM . InvalidXML
                $ "GetBucketLocation: unrecognized region " <> quote reg
