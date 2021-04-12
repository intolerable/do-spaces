{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Actions.GetObjectInfo
    ( GetObjectInfo(..)
    , ObjectInfoResponse
    ) where

import           Control.Monad.Catch       ( throwM )
import           Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )

import qualified Data.ByteString.Char8     as C
import           Data.Generics.Product     ( HasField(field) )
import qualified Data.Text.Encoding        as T
import           Data.Time                 ( defaultTimeLocale, parseTimeM )

import           GHC.Generics              ( Generic )

import           Lens.Micro                ( (^.) )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , ClientException(OtherError)
                 , Method(HEAD)
                 , Object
                 , ObjectMetadata(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils   ( eitherToMaybe, unquote )

import           Text.Read                 ( readMaybe )

-- | Get information about an 'Object'; the response does not contain the
-- object itself
data GetObjectInfo = GetObjectInfo { object :: Object, bucket :: Bucket }
    deriving ( Show, Eq, Generic )

type ObjectInfoResponse = ObjectMetadata

instance Action GetObjectInfo where
    type (SpacesResponse GetObjectInfo) = ObjectInfoResponse

    buildRequest spaces GetObjectInfo { .. } = SpacesRequestBuilder
        { bucket      = Just bucket
        , object      = Just object
        , method      = Just HEAD
        , body        = Nothing
        , queryString = Nothing
        , headers     = mempty
        , ..
        }

    consumeResponse raw = do
        metadata <- runMaybeT
            $ ObjectMetadata <$> (readLen =<< lookupHeader "Content-Length")
            <*> lookupHeader "Content-Type"
            <*> (readEtag =<< lookupHeader "Etag")
            <*> (readDate =<< lookupHeader "Last-Modified")
        case metadata of
            Just md -> return md
            Nothing -> throwM $ OtherError "Missing/malformed headers"
      where
        lookupHeader h = MaybeT . return $ lookup h (raw ^. field @"headers")

        readLen        = MaybeT . return . readMaybe @Int . C.unpack

        readDate       = MaybeT . return . parseAmzTime . C.unpack

        readEtag       =
            MaybeT . return . fmap unquote . eitherToMaybe . T.decodeUtf8'

        parseAmzTime   =
            parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %EZ"
