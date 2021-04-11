{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Network.DO.Spaces.Types
    ( -- * Spaces
      SpacesT(..)
    , runSpacesT
    , Spaces(..)
    , MonadSpaces
    , Action(..)
    , CredentialSource(..)
      -- * Making requests
    , SpacesRequest(..)
    , SpacesRequestBuilder(..)
    , RawBody
    , Method(..)
    , Region(..)
    , AccessKey(..)
    , SecretKey(..)
    , Canonicalized(..)
    , Computed(..)
    , StringToSign
    , Hashed
    , Signature
    , Credentials
    , Authorization
    , uncompute
      -- * Buckets and Objects
    , Object(..)
    , Bucket(..)
    , BucketInfo(..)
    , ID(..)
    , DisplayName
    , Owner(..)
    , ObjectInfo(..)
    , CannedACL(..)
      -- * Exceptions
    , SpacesException(..)
    ) where

import           Conduit                     ( ConduitT, MonadUnliftIO )

import           Control.Exception           ( Exception )
import           Control.Monad.Catch         ( MonadThrow )
import           Control.Monad.IO.Class      ( MonadIO )
import           Control.Monad.Reader        ( MonadReader
                                             , ReaderT(ReaderT, runReaderT)
                                             )

import           Data.ByteString             ( ByteString )
import           Data.Data                   ( Typeable )
import           Data.Kind                   ( Type )
import           Data.Text                   ( Text )
import           Data.Time                   ( UTCTime )

import           GHC.Generics                ( Generic )

import           Network.HTTP.Client.Conduit
                 ( HasHttpManager(..)
                 , Manager
                 , Request
                 , RequestBody
                 )
import           Network.HTTP.Types          ( Header, Query )

newtype SpacesT a = SpacesT (ReaderT Spaces IO a)
    deriving ( Generic, Functor, Applicative, Monad, MonadIO, MonadThrow
             , MonadReader Spaces, MonadUnliftIO )

runSpacesT :: SpacesT a -> Spaces -> IO a
runSpacesT (SpacesT x) = runReaderT x

type MonadSpaces m =
    (MonadReader Spaces m, MonadIO m, MonadUnliftIO m, MonadThrow m)

data Spaces = Spaces
    { accessKey :: AccessKey -- ^ Your DO access key
    , secretKey :: SecretKey -- ^ Your DO secret key
    , region    :: Region -- ^ The DO region
    , manager   :: Manager -- ^ HTTP 'Manager'
    }
    deriving ( Generic )

instance HasHttpManager Spaces where
    getHttpManager = manager

data SpacesRequest = SpacesRequest
    { request          :: Request -- ^ The actual HTTP 'Request'
    , spaces           :: Spaces -- ^ Your 'Spaces' configuration
    , headers          :: [Header]
      -- ^ Obligatory 'Header's that will be added to the request
    , method           :: Method -- ^ The HTTP 'Method'
    , payload          :: ByteString
      -- ^ A 'RequestBody' read into a strict 'ByteString'
    , payloadHash      :: Hashed -- ^ The SHA256 hash of the 'RequestBody'
    , canonicalRequest :: Canonicalized Request -- ^ The canonicalized HTTP 'Request'
    , time             :: UTCTime
    }
    deriving ( Generic )

data SpacesRequestBuilder = SpacesRequestBuilder
    { spaces      :: Spaces
    , body        :: Maybe RequestBody
    , method      :: Maybe Method
    , headers     :: [Header]
    , bucket      :: Maybe Bucket
    , object      :: Maybe Object
    , queryString :: Maybe Query
    }
    deriving ( Generic )

-- | DO regions where Spaces is available (only a subset of all regions)
data Region
    = NewYork -- ^ NYC3
    | Amsterdam -- ^ AMS3
    | SanFrancisco  -- ^ SFO3
    | Singapore -- ^ SGP1
    | Frankfurt -- ^ FRA1
    deriving ( Show, Eq, Generic )

-- | HTTP request methods, to avoid using @http-client@'s stringly-typed @Method@
-- synonym
data Method = GET | POST | PUT | DELETE
    deriving ( Show, Eq, Generic )

newtype Bucket = Bucket { unBucket :: Text }
    deriving ( Show, Eq, Generic )

data BucketInfo = BucketInfo { name :: Bucket, creationDate :: UTCTime }
    deriving ( Show, Eq, Generic )

newtype Object = Object { unObject :: Text }
    deriving ( Show, Eq, Generic )

data ObjectInfo = ObjectInfo
    { object       :: Object --
    , lastModified :: UTCTime
    , etag         :: Text
    , size         :: Int
    , owner        :: Owner
    }
    deriving ( Show, Eq, Generic )

data Owner = Owner { id' :: ID, displayName :: DisplayName }
    deriving ( Show, Eq, Generic )

newtype ID = ID { unID :: Int }
    deriving ( Show, Eq, Generic, Num )

type DisplayName = ID

-- | Represents some resource that has been canonicalized according to the
-- Spaces/AWS v4 spec
newtype Canonicalized a = Canonicalized { unCanonicalized :: ByteString }
    deriving ( Show, Eq, Generic )

-- | Different types of computed 'ByteString's
data ComputedTag = Hash | StrToSign | Sig | Cred | Auth
    deriving ( Show, Eq )

-- | A strict 'ByteString' that has been computed according to some part of
-- the AWS v4 spec
data Computed (a :: ComputedTag) where
    Hashed :: ByteString -> Computed 'Hash
    -- | Represents a \"string to sign\" that has been computed according to the
    -- Spaces/AWS v4 spec
    StringToSign :: ByteString -> Computed 'StrToSign
    -- | Signed hash of a 'Request' body, a 'SecretKey', and request information
    Signature :: ByteString -> Computed 'Sig
    -- | Signed hash of a 'Request' body, a 'SecretKey', and request information
    Credentials :: ByteString -> Computed 'Cred
    -- | Authorization string containing information about your 'AccessKey' and
    -- your request
    Authorization :: ByteString -> Computed 'Auth

deriving instance Show (Computed a)

deriving instance Eq (Computed a)

type StringToSign = Computed 'StrToSign

type Hashed = Computed 'Hash

type Signature = Computed 'Sig

type Credentials = Computed 'Cred

type Authorization = Computed 'Auth

-- | Extract the 'ByteString' from something 'Computed'
uncompute :: Computed a -> ByteString
uncompute = \case
    Hashed b        -> b
    StringToSign b  -> b
    Signature b     -> b
    Credentials b   -> b
    Authorization b -> b

-- | DO Spaces access key
newtype AccessKey = AccessKey { unAccessKey :: ByteString }
    deriving ( Show, Eq, Generic )

-- | DO Spaces secret key
newtype SecretKey = SecretKey { unSecretKey :: ByteString }
    deriving ( Show, Eq, Generic )

-- Generate a 'SpacesRequestBuilder' for a given type, settings the appropriate
-- specific 'Header's, etc..., for that type
class Action a where
    type SpacesResponse a :: Type

    buildRequest :: Spaces -> a -> SpacesRequestBuilder
    consumeResponse
        :: (MonadIO m, MonadThrow m) => RawBody -> m (SpacesResponse a)

type RawBody = ConduitT () ByteString IO ()

-- How to discover 'AccessKey's and 'SecretKey's when creating a new
-- 'Spaces' object
data CredentialSource
    = InEnv (Maybe (Text, Text)) -- ^ 'AccessKey' and 'SecretKey' env vars
    | Explicit AccessKey SecretKey -- ^ Provide both

-- | \"Canned\" access controls; Spaces doesn't support the full range offered
-- by s3
data CannedACL
    = Private -- ^ No unauthenticated public access
    | PublicRead -- ^ Unauthenticated public read access
    deriving ( Eq, Show )

-- | An exception generated within the 'Spaces' client
data SpacesException
    = InvalidRequest Text
    | InvalidXML Text
    | MissingKeys Text
    | HTTPStatus Int ByteString
    | OtherError Text
    deriving ( Show, Eq, Generic, Typeable )

instance Exception SpacesException
