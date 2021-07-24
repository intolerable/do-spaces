{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Network.DO.Spaces
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Interacting with DigitalOcean's Spaces API, a (largely) s3-compatible object
-- storage platform. This module exports actions to create a 'Spaces' client
-- configuration as well as several convenience actions. Most of the transactions
-- exposed through the Spaces REST API are supported here, including CRUD operations
-- on buckets and objects, bucket CORS configuration, and manipulating ACLs.
--
-- See the README in this repository for more information on using this library
--
module Network.DO.Spaces
    ( runSpaces
    , newSpaces
      -- * Convenience actions
      -- $conv
      -- ** Object operations
    , uploadObject
    , multipartObject
    , uploadFile
    , getObject
    , getObjectSinkFile
    , getObjectInfo
    , copyObject
    , copyObjectWithin
    , overwriteObject
    , deleteObject
    , getObjectACLs
    , setObjectACLs
      -- ** Bucket operations
    , createBucket
    , deleteBucket
    , getBucketLocation
    , listAllBuckets
    , listBucket
    , listBucketGrouped
    , listBucketRec
    , getBucketCORS
    , deleteBucketCORS
    , setBucketCORS
    , getBucketACLs
    , setBucketACLs
    , getBucketLifecycleRules
    , setBucketLifecycleRules
    , deleteBucketLifecycleRules
      -- * Re-exports
    , Spaces
    , SpacesResponse
    , SpacesMetadata
    , MonadSpaces
    , Bucket
    , mkBucket
    , Object
    , mkObject
    , Region(..)
    , AccessKey(..)
    , SecretKey(..)
    , CredentialSource(..)
    , Profile
    , CORSRule
    , mkCORSRule
    , Grant(..)
    , Grantee(..)
    , Permission(..)
    , LifecycleID
    , mkLifecycleID
    , SpacesException
    , ClientException(..)
    , APIException(..)
    ) where

import           Conduit

import           Control.Monad               ( void )
import           Control.Monad.Catch         ( MonadCatch(catch) )

import           Data.Bool                   ( bool )
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import           Data.Coerce                 ( coerce )
import           Data.Conduit.Binary         ( sinkLbs )
import           Data.Conduit.List           ( consume )
import           Data.Foldable               ( asum )
import           Data.Generics.Product       ( HasField(field) )
import qualified Data.Ini.Config             as I
import           Data.Maybe                  ( fromMaybe )
import           Data.Sequence               ( Seq )
import qualified Data.Text                   as T
import           Data.Text                   ( Text )
import qualified Data.Text.Lazy              as LT

import           Lens.Micro                  ( (<&>), (^.) )

import           Network.DO.Spaces.Actions
import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import           Network.HTTP.Client.TLS     ( getGlobalManager )
import           Network.Mime
                 ( MimeType
                 , defaultMimeMap
                 , defaultMimeType
                 , mimeByExt
                 )

import qualified System.FilePath             as F

import           UnliftIO.Directory
import           UnliftIO.Environment

-- | Perform a transaction using your 'Spaces' client configuration. Note that
-- this does not perform any exception handling; if caught at the lower level,
-- exceptions are generally re-thrown as 'SpacesException's
runSpaces :: Spaces -> SpacesT m a -> m a
runSpaces = flip runSpacesT

-- | Create a new 'Spaces' by specifying a method to retrieve the region and
-- your credentials.
--
-- 'Discover' will first try to find a credentials file (see the notes on 'FromFile'
-- below) in @~/.aws/credentials@ or @$XDG_CONFIG_HOME/do-spaces/credentials@, in
-- that order, using the @[default]@ profile. Failing that, it will try the equivalent
-- of @FromEnv Nothing@ (see the notes below).
--
-- 'FromFile' expects a configuration file in the same format as AWS credentials
-- files, with the same field names. For example:
--
-- > [default]
-- > aws_access_key_id=AKIAIOSFODNN7EXAMPLE
-- > aws_secret_access_key=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
-- > aws_default_region=fra1
--
-- When provided with @Nothing@, 'FromEnv' will look up the following environment
-- variables to find your region and keys:
--
--     * For the 'Region':
--
--         - @AWS_DEFAULT_REGION@
--         - @SPACES_DEFAULT_REGION@
--
--     * For the 'AccessKey':
--
--         - @AWS_ACCESS_KEY_ID@
--         - @SPACES_ACCESS_KEY_ID@
--         - @SPACES_ACCESS_KEY@
--
--     * For the 'SecretKey':
--
--         - @AWS_SECRET_ACCESS_KEY@
--         - @SPACES_SECRET_ACCESS_KEY@
--         - @SPACES_SECRET_KEY@
--
-- Alternatively, you can directly specify a tuple of environment variables to
-- search for.
--
-- You can also choose to provide the region and both keys yourself with 'Explicit'
--
newSpaces :: (MonadThrow m, MonadIO m) => CredentialSource -> m Spaces
newSpaces cs = do
    manager <- liftIO getGlobalManager
    (region, accessKey, secretKey) <- liftIO $ source cs
    pure Spaces { .. }

source :: (MonadIO m, MonadCatch m)
       => CredentialSource
       -> m (Region, AccessKey, SecretKey)
source = \case
    Discover -> catch @_ @ClientException tryFile $ const tryEnv
      where
        tryFile = do
            cfgDir <- getXdgDirectory XdgConfig "do-spaces"
            findFile [ ".aws", cfgDir ] "credentials" >>= \case
                Nothing ->
                    throwM $ ConfigurationError "No credentials file found"
                Just fp -> source $ FromFile fp Nothing

        tryEnv  = source $ FromEnv Nothing

    Explicit region ak sk -> pure (region, ak, sk)

    FromEnv (Just (region, a, s)) -> ensureVars
        =<< (,,) <$> lookupVar region <*> lookupVar a <*> lookupVar s

    FromEnv Nothing -> do
        region <- lookupVars [ "AWS_DEFAULT_REGION", "SPACES_DEFAULT_REGION" ]
        ak <- lookupVars [ "AWS_ACCESS_KEY_ID"
                         , "SPACES_ACCESS_KEY_ID"
                         , "SPACES_ACCESS_KEY"
                         ]
        sk <- lookupVars [ "AWS_SECRET_ACCESS_KEY"
                         , "SPACES_SECRET_ACCESS_KEY"
                         , "SPACES_SECRET_KEY"
                         ]
        ensureVars (region, ak, sk)

    FromFile fp profile -> do
        contents <- LT.toStrict
            <$> liftIO (runConduitRes (sourceFile fp
                                       .| decodeUtf8C
                                       .| sinkLazy))
        either (throwM . ConfigurationError . T.pack) ensureVars
            $ I.parseIniFile contents parseConf
      where
        parseConf = I.section (fromMaybe "default" profile)
            $ (,,)
            <$> (fmap (T.unpack . T.toLower)
                 <$> I.fieldMb "aws_default_region")
            <*> (fmap T.unpack <$> I.fieldMb "aws_access_key_id")
            <*> (fmap T.unpack <$> I.fieldMb "aws_secret_access_key")
  where
    lookupVars xs = asum <$> sequence (lookupEnv <$> xs)

    lookupVar     = lookupEnv . T.unpack

    ensureVars    = \case
        (Just r, Just a, Just s) -> do
            reg <- slugToRegion r
            pure (reg, coerce $ C.pack a, coerce $ C.pack s)
        (_, _, _)                -> throwM . ConfigurationError
            $ "Missing acces/secret keys and/or region"

-- | Upload an 'Object' within a single request
uploadObject :: MonadSpaces m
             => Maybe MimeType
             -> Bucket
             -> Object
             -> BodyBS m
             -> m (SpacesResponse UploadObject)
uploadObject contentType bucket object rbody = do
    body <- RequestBodyLBS <$> runConduit (rbody .| sinkLbs)
    runAction KeepMetadata
              UploadObject { optionalHeaders = defaultUploadHeaders, .. }

-- | Initiate and complete a multipart upload, using default 'UploadHeaders'.
-- If a 'SpacesException' is thrown while performing the transaction, an attempt
-- will be made to run a 'CancelMultipart' request, and the exception will be
-- rethrown
multipartObject
    :: MonadSpaces m
    => Maybe MimeType
    -> Bucket
    -> Object
    -> Int
    -> BodyBS m
    -> m (SpacesResponse CompleteMultipart)
multipartObject contentType bucket object size body
    | size < 5242880 = throwM
        $ InvalidRequest "multipartObject: Chunk size must be greater than/equal to 5MB"
    | otherwise = do
        session <- beginMultipart <&> (^. field @"result" . field @"session")
        catch @_ @SpacesException (run session) $ \e -> do
            void . runAction NoMetadata $ CancelMultipart session
            throwM e
  where
    run session = completeMultipart session
        =<< runConduit (body .| inChunks .| putPart session .| consume)

    beginMultipart = runAction NoMetadata
                               BeginMultipart
                               { optionalHeaders = defaultUploadHeaders, .. }

    completeMultipart session tags = runAction KeepMetadata
        $ CompleteMultipart session (zip [ 1 .. ] tags)

    putPart session = go 1
      where
        go !n = awaitForever $ \v -> do
            let pieceBody  = RequestBodyLBS v
                uploadPart =
                    runAction NoMetadata $ UploadPart session n pieceBody

            etag <- lift $ uploadPart <&> (^. field @"result" . field @"etag")
            yield etag >> go (n + 1)

    inChunks = loop 0 []
      where
        loop !n chunk = await >>= maybe (yieldChunk chunk) go
          where
            go bs = bool (loop len newChunk)
                         (yieldChunk newChunk >> loop 0 [])
                         (size <= len)
              where
                len      = C.length bs + n

                newChunk = bs : chunk

        yieldChunk = yield . LB.fromChunks . reverse

-- | Upload a file's contents as an 'Object'. This will attempt to set the
-- correct 'Network.Mime.MimeType' based on the file extension
uploadFile :: forall m.
           MonadSpaces m
           => Bucket
           -> Object
           -> FilePath
           -> m (SpacesResponse UploadObject)
uploadFile bucket object fp = withSourceFile @_ @m fp $ \body ->
    uploadObject (Just mtype) bucket object body
  where
    mtype =
        mimeByExt defaultMimeMap defaultMimeType . T.pack $ F.takeFileName fp

-- | Get information about an 'Object' (does not retrieve the body of the object)
getObjectInfo
    :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse GetObjectInfo)
getObjectInfo bucket object = runAction KeepMetadata GetObjectInfo { .. }

-- | Get an 'Object' (retrieves the actual body of the object)
getObject :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse GetObject)
getObject bucket object = runAction KeepMetadata GetObject { .. }

-- | Get an 'Object'\'s data and write it to the provided 'FilePath'
getObjectSinkFile :: MonadSpaces m => Bucket -> Object -> FilePath -> m ()
getObjectSinkFile bucket object fp = do
    objectData <- getObject bucket object
        <&> (^. field @"result" . field @"objectData")
    runConduitRes $ sourceLazy objectData .| sinkFileCautious fp

-- | Copy an 'Object' from one 'Bucket' to another; this chooses a number of
-- defaults to represent the most common cases and avoid a preponderance of
-- parameters. 'Object's are copied using default ACLs with the COPY metadata
-- directive.
--
-- If you'd like to use a specfic 'CannedACL' or 'MetadataDirective', use
-- 'CopyObject' directly with 'runAction'
copyObject :: MonadSpaces m
           => Bucket -- ^ Source 'Bucket'
           -> Bucket -- ^ Destination 'Bucket'
           -> Object -- ^ Source 'Object'
           -> Object -- ^ Destination 'Object'
           -> m (SpacesResponse CopyObject)
copyObject srcBucket destBucket srcObject destObject =
    runAction KeepMetadata CopyObject { .. }
  where
    acl               = Nothing

    metadataDirective = Copy

-- | Copy an 'Object' within the same 'Bucket', using defaults for the
-- 'MetadataDirective' and 'CannedACL'
copyObjectWithin :: MonadSpaces m
                 => Bucket
                 -> Object -- ^ Source 'Object'
                 -> Object -- ^ Destination 'Object'
                 -> m (SpacesResponse CopyObject)
copyObjectWithin srcBucket srcObject destObject =
    runAction KeepMetadata CopyObject { .. }
  where
    acl               = Nothing

    metadataDirective = Copy

    destBucket        = srcBucket

-- | Copy an 'Object' to itself, overwriting its associated metadata
overwriteObject
    :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse CopyObject)
overwriteObject srcBucket srcObject = runAction KeepMetadata CopyObject { .. }
  where
    acl               = Nothing

    metadataDirective = Copy

    destBucket        = srcBucket

    destObject        = srcObject

-- | Delete a single 'Object'
deleteObject
    :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse DeleteObject)
deleteObject bucket object = runAction KeepMetadata DeleteObject { .. }

-- | Get an 'Object'\'s Access Control Lists
getObjectACLs
    :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse GetObjectACLs)
getObjectACLs bucket object = runAction KeepMetadata GetObjectACLs { .. }

-- | Set an 'Object'\'s Access Control Lists
setObjectACLs :: MonadSpaces m
              => Bucket
              -> Object
              -> Owner
              -> [Grant]
              -> m (SpacesResponse SetObjectACLs)
setObjectACLs bucket object owner acls =
    runAction KeepMetadata SetObjectACLs { .. }

-- | Create a new 'Bucket'
createBucket :: MonadSpaces m
             => Bucket
             -> Maybe Region -- ^ Overrides the 'Region' in your 'Spaces'
                             -- configuration
             -> Maybe CannedACL
             -> m (SpacesResponse CreateBucket)
createBucket bucket region acl = runAction KeepMetadata CreateBucket { .. }

-- | Delete a 'Bucket'
deleteBucket :: MonadSpaces m => Bucket -> m (SpacesResponse DeleteBucket)
deleteBucket bucket = runAction KeepMetadata DeleteBucket { .. }

-- | Get the location ('Region') of a 'Bucket'
getBucketLocation
    :: MonadSpaces m => Bucket -> m (SpacesResponse GetBucketLocation)
getBucketLocation bucket = runAction KeepMetadata GetBucketLocation { .. }

-- | List every 'Bucket' associated with your Spaces account
listAllBuckets :: MonadSpaces m => m (SpacesResponse ListAllBuckets)
listAllBuckets = runAction KeepMetadata ListAllBuckets

-- | List the 'Object's of a 'Bucket', without grouping, delimiting, or limiting
-- the keys (i.e. list all 'Object's non-hierarchically, up to the Spaces limit)
listBucket :: MonadSpaces m => Bucket -> m (SpacesResponse ListBucket)
listBucket bucket = runAction KeepMetadata ListBucket { .. }
  where
    delimiter = Nothing

    marker    = Nothing

    maxKeys   = Nothing

    prefix    = Nothing

-- | List the 'Object's of a 'Bucket', using a delimiter and prefix to group
-- objects. For example @\/@ can be used as a delimiter to treat objects as
-- directories within the bucket, which can further be combined with a text
-- prefix
listBucketGrouped :: MonadSpaces m
                  => Bucket
                  -> Char -- ^ Delimiter
                  -> Text -- ^ Prefix used to group object keys
                  -> m (SpacesResponse ListBucket)
listBucketGrouped bucket delimiter prefix =
    runAction KeepMetadata
              ListBucket
              { delimiter = Just delimiter, prefix = Just prefix, .. }
  where
    maxKeys = Nothing

    marker  = Nothing

-- | Recursively list /all/ 'Object's in a 'Bucket', calling 'ListBucket' until
-- @isTruncated@ is @False@. This operation may take some time, depending on the
-- total number of objects in your bucket
listBucketRec :: MonadSpaces m => Bucket -> m (Seq ObjectInfo)
listBucketRec bucket = go mempty Nothing
  where
    go os marker = do
        listed <- runAction NoMetadata
            $ ListBucket
            { delimiter = Nothing, maxKeys = Nothing, prefix = Nothing, .. }
        let r           = listed ^. field @"result"
            isTruncated = r ^. field @"isTruncated"
            objects     = r ^. field @"objects"
            nextMarker  = r ^. field @"nextMarker"
        case nextMarker of
            Just _
                | isTruncated -> go (os <> objects) nextMarker
                | otherwise -> pure $ os <> objects
            Nothing -> pure $ os <> objects

-- | Get the 'CORSRule's configured for a given 'Bucket'
getBucketCORS :: MonadSpaces m => Bucket -> m (SpacesResponse GetBucketCORS)
getBucketCORS bucket = runAction KeepMetadata $ GetBucketCORS { .. }

-- | Set 'CORSRule's for a given 'Bucket'
setBucketCORS :: MonadSpaces m
              => Bucket
              -> [CORSRule]
              -> m (SpacesResponse SetBucketCORS)
setBucketCORS bucket rules = runAction KeepMetadata $ SetBucketCORS { .. }

-- | Delete the existing configured 'CORSRule's for a given 'Bucket'
deleteBucketCORS
    :: MonadSpaces m => Bucket -> m (SpacesResponse DeleteBucketCORS)
deleteBucketCORS bucket = runAction KeepMetadata $ DeleteBucketCORS { .. }

-- | Get a 'Bucket'\'s Access Control Lists
getBucketACLs :: MonadSpaces m => Bucket -> m (SpacesResponse GetBucketACLs)
getBucketACLs bucket = runAction KeepMetadata $ GetBucketACLs { .. }

-- | Set a 'Bucket'\'s Access Control Lists. Spaces only allows a limited subset
-- of s3 ACLs at the moment. It may be preferable to use a 'CannedACL' when
-- creating new resources rather than using this action, which is provided
-- for the sake of completeness.
--
-- Note that to allow public read-only access to your bucket, you /must/
-- simultaneously set full owner control.
setBucketACLs :: MonadSpaces m
              => Bucket
              -> [Grant]
              -> Owner
              -> m (SpacesResponse SetBucketACLs)
setBucketACLs bucket acls owner =
    runAction KeepMetadata $ SetBucketACLs { .. }

-- | Get a 'Bucket'\'s 'LifecycleRule' configuration . Note that unless you
-- have explicitly configured lifecycle rules, this will fail with a 404
-- status and an error code of @NoSuchLifecycleConfiguration@
getBucketLifecycleRules
    :: MonadSpaces m => Bucket -> m (SpacesResponse GetBucketLifecycle)
getBucketLifecycleRules bucket =
    runAction KeepMetadata $ GetBucketLifecycle { .. }

-- | Set a 'Bucket'\'s 'LifecycleRule' configuration
setBucketLifecycleRules :: MonadSpaces m
                        => Bucket
                        -> [LifecycleRule]
                        -> m (SpacesResponse SetBucketLifecycle)
setBucketLifecycleRules bucket rules =
    runAction KeepMetadata $ SetBucketLifecycle { .. }

-- | Delete a 'Bucket'\'s 'LifecycleRule' configuration
deleteBucketLifecycleRules
    :: MonadSpaces m => Bucket -> m (SpacesResponse DeleteBucketLifecycle)
deleteBucketLifecycleRules bucket =
    runAction KeepMetadata $ DeleteBucketLifecycle { .. }
--
-- $conv
-- The following are convenience actions. In most cases, each action is the same
-- as applying 'runAction' to a type that implements the 'Action' typeclass.
-- Information about the response is retained ('SpacesMetadata') in each action.
-- For instance:
--
-- > deleteBucket myBucket
--
-- is the equivalent of
--
-- > runAction KeepMetadata DeleteBucket { bucket = myBucket }
--
-- All of the underlying instances of 'Action' are exposed and can be imported from
-- "Network.DO.Spaces.Actions" and its sub-modules. The convenience actions exposed
-- in the present module attempt to choose sane defaults where applicable.
--
-- The only major exception to the above are actions which involve uploading object
-- data to Spaces. In the case of 'uploadObject', the action converts its 'BodyBS'
-- argument to a 'RequestBodyLBS'. Should you choose to directly construct
-- 'UploadObject', you must do this manually. 'multipartObject' is more complicated,
-- and takes care of chunking the request body, sending each individual request,
-- and completing the multipart request
--
-- In addition to convenience wrappers around 'Action' instances, this module exports
-- several actions which may be of use, including sinking remote 'Object' data into
-- a file, uploading the contents of a file as an 'Object', and recursively listing
-- the entire contents of a 'Bucket'
--
