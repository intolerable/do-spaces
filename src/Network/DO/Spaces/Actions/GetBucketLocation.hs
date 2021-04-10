{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Actions.GetBucketLocation
    ( GetBucketLocation(..)
    , GetBucketLocationResponse(..)
    ) where

import           Conduit                 ( MonadThrow(throwM) )

import           Data.ByteString         ( ByteString )

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , Region(..)
                 , SpacesException(OtherError)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( xmlAttrError, xmlDocCursor )
import qualified Network.HTTP.Types      as H

import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($.//), (&/), (&|) )
import qualified Data.Text as T

-- | Query the location (the 'Region') of a 'Bucket'
data GetBucketLocation = GetBucketLocation
    { bucket :: Bucket
      -- ^ The name of the 'Bucket' whose location you'd like to retrieve
    }
    deriving ( Show, Eq, Generic )

data GetBucketLocationResponse = GetBucketLocationResponse
    { locationConstraint :: Region -- ^ The 'Region' of the queried 'Bucket'
    }
    deriving ( Show, Eq, Generic )

instance Action GetBucketLocation where
    type (SpacesResponse GetBucketLocation) = GetBucketLocationResponse

    buildRequest spaces GetBucketLocation { .. } = SpacesRequestBuilder
        { bucket      = Just bucket
        , method      = Nothing
        , body        = Nothing
        , object      = Nothing
        , headers     = mempty
        , queryString = Just
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
            reg    -> throwM . OtherError $ "Unrecognized region: " <> reg
