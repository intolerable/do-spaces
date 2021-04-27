{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.DO.Spaces.Types
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Network.DO.Spaces.Types
    ( -- * Spaces
      SpacesT(..)
    , runSpacesT
    , Spaces(..)
    , MonadSpaces
    , Action(..)
    , CredentialSource(..)
      -- * Requests and responses
    , SpacesRequest(..)
    , SpacesResponse(..)
    , SpacesRequestBuilder(..)
    , SpacesMetadata(..)
    , WithMetadata(..)
    , RawResponse(..)
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
    , ETag
    , CacheControl
    , ContentDisposition
    , ContentEncoding
    , UserMetadata
    , UploadHeaders(..)
    , BodyBS
      -- * Buckets and Objects
    , Object(..)
    , mkObject
    , Bucket(..)
    , mkBucket
    , BucketInfo(..)
    , OwnerID(..)
    , DisplayName
    , Owner(..)
    , ObjectInfo(..)
    , ObjectMetadata(..)
    , CannedACL(..)
      -- * Exceptions
    , ClientException(..)
    , SpacesException(..)
    , APIException(..)
    ) where

import           Conduit                      ( ConduitT, MonadUnliftIO )

import           Control.Exception
                 ( Exception(toException, fromException)
                 , SomeException
                 )
import           Control.Monad.Catch          ( MonadCatch
                                              , MonadThrow(throwM)
                                              )
import           Control.Monad.IO.Class       ( MonadIO )
import           Control.Monad.Reader         ( MonadReader
                                              , ReaderT(ReaderT, runReaderT)
                                              )

import           Data.ByteString              ( ByteString )
import qualified Data.ByteString.Lazy         as LB
import           Data.Char                    ( isAlpha, isDigit, toLower )
import           Data.Data                    ( Typeable )
import qualified Data.Generics.Product.Fields as GL
import           Data.Ix                      ( inRange )
import           Data.Kind                    ( Type )
import           Data.Text                    ( Text )
import qualified Data.Text                    as T
import           Data.Time                    ( UTCTime )
import           Data.Typeable                ( cast )

import           GHC.Generics                 ( Generic )

import           Network.HTTP.Client.Conduit
                 ( HasHttpManager(..)
                 , Manager
                 , Request
                 , RequestBody
                 )
import           Network.HTTP.Types           ( Header, Query )
import           Network.HTTP.Types.Status    ( Status )
import           Network.Mime                 ( MimeType )

newtype SpacesT m a = SpacesT (ReaderT Spaces m a)
    deriving ( Generic, Functor, Applicative, Monad, MonadIO, MonadThrow
             , MonadCatch, MonadReader Spaces, MonadUnliftIO )

runSpacesT :: SpacesT m a -> Spaces -> m a
runSpacesT (SpacesT x) = runReaderT x

type MonadSpaces m =
    (MonadReader Spaces m, MonadIO m, MonadUnliftIO m, MonadCatch m)

-- | A client for interacting with the DO Spaces API
data Spaces = Spaces
    { accessKey :: AccessKey -- ^ Your DO Spaces access key
    , secretKey :: SecretKey -- ^ Your DO Spaces secret key
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
    , payloadHash      :: Hashed
      -- ^ The SHA256 hash of the 'RequestBody' contents
    , canonicalRequest :: Canonicalized Request
      -- ^ The canonicalized HTTP 'Request'
    , time             :: UTCTime
    }
    deriving ( Generic )

data SpacesRequestBuilder = SpacesRequestBuilder
    { spaces         :: Spaces
    , body           :: Maybe RequestBody
    , method         :: Maybe Method
    , headers        :: [Header]
    , bucket         :: Maybe Bucket
    , object         :: Maybe Object
    , queryString    :: Maybe Query
    , subresources   :: Maybe Query
    , overrideRegion :: Maybe Region
      -- ^ Certain operations, currently only 'Network.DO.Spaces.CreateBucket',
      -- should be able to override the region configured in the 'Spaces'
      -- client
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
data Method = GET | POST | PUT | DELETE | HEAD
    deriving ( Show, Eq, Generic )

-- | The name of a single storage bucket
newtype Bucket = Bucket { unBucket :: Text }
    deriving ( Show, Eq, Generic )

-- | Smart constructor for 'Bucket's; names must conform to the following rules:
--
--      * They must be between 3 and 63 characters in length
--      * They may only contain lowercase letters, digits, dots, and hyphens
--      * They must begin and end in a number or letter
-- See more at:
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html>.
--
-- This function ensures that names are valid and will also convert the 'Text'
-- to lowercase
mkBucket :: MonadThrow m => Text -> m Bucket
mkBucket t
    | not $ inRange (3, 63) len =
        bucketErr "Name must be between 3-63 characters"
    | not $ T.all (or . okChars) t = bucketErr
        $ mconcat [ "Names may only consist of "
                  , "lowercase letters, digits, dots, and hyphens"
                  ]
    -- Yes, partial. But the length of the Text has already been
    -- established and using uncons/unsnoc would be redundant
    | T.head t `elem` [ '.', '-' ] =
        bucketErr "Name must begin with a letter or digit"
    | T.last t
        `elem` [ '.', '-' ] = bucketErr "Name must end with a letter or digit"
    | otherwise = return . Bucket $ T.map toLower t
  where
    len         = T.length t

    -- isAlphaNum may select non-ASCII digits, but isDigit doesn't.
    -- It's better to check isDigit and isAlpha separately
    okChars c = [ ('.' ==), ('-' ==), isDigit, isAlpha ] <*> [ c ]

    bucketErr e = throwM . OtherError $ "Bucket: " <> e

-- | Information about a single 'Bucket'
data BucketInfo = BucketInfo { name :: Bucket, creationDate :: UTCTime }
    deriving ( Show, Eq, Generic )

-- | The name of a \"key\", in AWS parlance
newtype Object = Object { unObject :: Text }
    deriving ( Show, Eq, Generic )

-- | Smart constructor for 'Object's; names must not be empty
mkObject :: MonadThrow m => Text -> m Object
mkObject "" = throwM . OtherError $ "Object: Name must not be empty"
mkObject x  = return $ Object x

-- | Information about a single 'Object', returned when listing a 'Bucket'\'s
-- contents
data ObjectInfo = ObjectInfo
    { object       :: Object
    , lastModified :: UTCTime
    , etag         :: ETag
    , size         :: Int -- ^ Size in bytes
    , owner        :: Owner
    }
    deriving ( Show, Eq, Generic )

-- | Metadata returned when querying information about an 'Object'
data ObjectMetadata = ObjectMetadata
    { contentLength :: Int -- ^ length in bytes
    , contentType   :: MimeType
    , etag          :: ETag
    , lastModified  :: UTCTime
    }
    deriving ( Show, Eq, Generic )

-- | The resource owner
data Owner = Owner { id' :: OwnerID, displayName :: DisplayName }
    deriving ( Show, Eq, Generic )

-- | The ID of an 'Owner'; also serves as a display name in Spaces
newtype OwnerID = OwnerID { unOwnerID :: Int }
    deriving ( Show, Eq, Generic, Num )

-- | The display name is always equivalent to the owner's ID; Spaces includes
-- it for AWS compatibility
type DisplayName = OwnerID

-- | MD5 hash of an 'Object'
type ETag = Text

-- | Optional headers when uploading objects
data UploadHeaders = UploadHeaders
    { acl                :: Maybe CannedACL
    , cacheControl       :: Maybe CacheControl
    , contentDisposition :: Maybe ContentDisposition
    , contentEncoding    :: Maybe ContentEncoding
    , metadata           :: UserMetadata
    }
    deriving ( Show, Eq, Generic )

-- | @Cache-Control@ request header value
type CacheControl = Text

-- | @Content-Disposition@ request header value
type ContentDisposition = Text

-- | @Content-Encoding@ request header value
type ContentEncoding = Text

-- | Arbitrary key-value pairs supplied by the user, for use in @PUT@ or @POST@
-- requests. Each pair expands into @x-amz-meta-*@, e.g.
-- @x-amz-meta-s3cmd-attrs: uid:1000/gname:asb...@
type UserMetadata = [(Text, Text)]

-- | Represents some resource that has been canonicalized according to the
-- Spaces/AWS v4 spec
newtype Canonicalized a = Canonicalized { unCanonicalized :: ByteString }
    deriving ( Show, Eq, Generic )

-- | Different types of computed 'ByteString's
data ComputedTag = Hash | StrToSign | Sig | Cred | Auth
    deriving ( Show, Eq )

-- | A strict 'ByteString' that has been computed according to some part of
-- the AWS v4 spec. The AWS v4 signature is calculated from a series of
-- interdependent computations. It would be possible to represent these all as
-- 'ByteString's; this approach, however, would make it easy to confuse two
-- sequences that are not semantically equivalent, leading to the generation of
-- malformed singatures. The promiscuous use of 'ByteString's also makes for
-- unclear type signatures. Using a GADT with type synonyms is simpler than
-- creating a @newtype@ for each type of computation
data Computed (a :: ComputedTag) where
    Hashed :: ByteString -> Computed 'Hash
    -- | Represents a \"string to sign\" that has been computed according to the
    -- Spaces/AWS v4 spec
    StringToSign :: ByteString -> Computed 'StrToSign
    -- | Signed hash of a 'Request' body, a 'SecretKey', and request information
    Signature :: ByteString -> Computed 'Sig
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

-- | Spaces access key
newtype AccessKey = AccessKey { unAccessKey :: ByteString }
    deriving ( Show, Eq, Generic )

-- | Spaces secret key
newtype SecretKey = SecretKey { unSecretKey :: ByteString }
    deriving ( Show, Eq, Generic )

-- Generate a 'SpacesRequestBuilder' for a given type, settings the appropriate
-- specific 'Header's, etc..., for that type
class Monad m => Action m a where
    type ConsumedResponse a :: Type

    buildRequest :: a -> m SpacesRequestBuilder
    consumeResponse :: RawResponse m -> m (ConsumedResponse a)

-- A response, before being transformed into a 'ConsumedResponse'
data RawResponse m = RawResponse { headers :: [Header], body :: BodyBS m }
    deriving ( Generic )

-- | A request or response body
type BodyBS m = ConduitT () ByteString m ()

-- | Metadata and other response information returned from each Spaces API
-- transaction; it can be helpful to retain this at times
data SpacesMetadata = SpacesMetadata
    { requestID :: Maybe RequestID
      -- ^ Unique ID assigned to your request. This is not included in all
      -- responses
    , date      :: Maybe UTCTime
    , status    :: Status -- ^ HTTP status
    }
    deriving ( Show, Eq, Generic )

-- | Whether or not to retain 'SpacesMetadata' when consuming responses
data WithMetadata = KeepMetadata | NoMetadata
    deriving ( Show, Eq, Generic )

-- | A 'ConsumedResponse' with optional 'SpacesMetadata'
data SpacesResponse a = SpacesResponse
    { result   :: ConsumedResponse a
      -- ^ A 'Response' consumed by an 'Action' instance
    , metadata :: Maybe SpacesMetadata
      -- ^ 'SpacesMetadata', the retention of which can be controlled using
      -- 'WithMetadata'
    }
    deriving ( Generic )

deriving instance (Show (ConsumedResponse a)) => Show (SpacesResponse a)

-- This instance is necessary to make the polymorphic @result@ field work with
-- HasField
instance {-# OVERLAPPING #-}( GL.HasField' name (SpacesResponse a) s
                            , s ~ t
                            , a ~ b
                            )
    => GL.HasField name (SpacesResponse a) (SpacesResponse b) s t where
    field = GL.field' @name

-- | A unique ID that is assigned to each request
type RequestID = Text

-- How to discover 'AccessKey's and 'SecretKey's when creating a new
-- 'Spaces' object
data CredentialSource
    = InEnv (Maybe (Text, Text)) -- ^ 'AccessKey' and 'SecretKey' env vars
    | Explicit AccessKey SecretKey -- ^ Provide both

-- | \"Canned\" access controls; Spaces doesn't support the full range offered
-- by s3
data CannedACL
    = Private -- ^ No unauthenticated public access
    | PublicRead -- ^ Unauthenticated public read access permitted
    deriving ( Eq, Show )

-- | The base 'Exception' type for both 'ClientException's and 'APIException's
data SpacesException = forall e. Exception e => SpacesException e

instance Show SpacesException where
    show (SpacesException e) = show e

instance Exception SpacesException

spsExToException :: Exception e => e -> SomeException
spsExToException = toException . SpacesException

spsExFromException :: Exception e => SomeException -> Maybe e
spsExFromException e = do
    SpacesException x <- fromException e
    cast x

-- | An exception generated within the 'Spaces' client
data ClientException
    = InvalidRequest Text
    | InvalidXML Text
    | MissingKeys Text
      -- | This includes the raw 'Response' body, read into a
      -- lazy 'LB.ByteString'
    | HTTPStatus Status LB.ByteString
    | OtherError Text
    deriving ( Show, Eq, Generic, Typeable )

instance Exception ClientException where
    toException = spsExToException

    fromException = spsExFromException

-- | An s3-compatible API error response, sent as XML
data APIException = APIException
    { status    :: Status -- ^ HTTP 'Status'
    , code      :: Text -- ^ The s3 error code type
    , requestID :: RequestID -- ^ The unique ID of the request
    , hostID    :: Text
    }
    deriving ( Show, Eq, Generic, Typeable )

instance Exception APIException where
    toException = spsExToException

    fromException = spsExFromException
