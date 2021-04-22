{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
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
-- on buckets and objects
--
module Network.DO.Spaces
    ( runSpaces
    , newSpaces
      -- * Convenience actions
      -- $conv
      -- ** Object operations
    , multipartObject
    , uploadObject
    , getObject
    , getObjectInfo
    , copyObject
    , deleteObject
      -- ** Bucket operations
    , createBucket
    , deleteBucket
    , getBucketLocation
    , listAllBuckets
    , listBucket
      -- * Type re-exports
    , Bucket
    , mkBucket
    , Object
    , mkObject
    , Region(..)
    , CredentialSource(..)
    ) where

import           Conduit
                 ( (.|)
                 , await
                 , runConduit
                 , yield
                 )

import           Control.Monad               ( void )
import           Control.Monad.Catch         ( MonadThrow(throwM) )
import           Control.Monad.Catch.Pure    ( MonadCatch(catch) )
import           Control.Monad.IO.Class      ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class  ( ask )

import           Data.Bool                   ( bool )
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import           Data.Conduit.Binary         ( sinkLbs )
import           Data.Conduit.List           ( consume )
import           Data.Foldable               ( asum )
import           Data.Generics.Product       ( HasField'(field') )
import qualified Data.Text                   as T
import           Data.Text                   ( Text )

import           Lens.Micro                  ( (<&>), (^.) )

import           Network.DO.Spaces.Actions
import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils     ( defaultUploadHeaders )
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import           Network.HTTP.Client.TLS     ( getGlobalManager )
import           Network.Mime                ( MimeType )

import           System.Environment          ( lookupEnv )

-- | Perform a transaction using your 'Spaces' client configuration. Note that
-- this does /not/ perform any exception handling; if caught at the lower level,
-- exceptions are generally re-thrown as 'SpacesException's
--
-- To run a 'SpacesT' action with arguments in the opposite order, you can use
-- 'runSpacesT' directly
runSpaces :: Spaces -> SpacesT m a -> m a
runSpaces = flip runSpacesT

-- | Create a new 'Spaces' from your credentials and a 'Region'
newSpaces
    :: (MonadThrow m, MonadIO m) => Region -> CredentialSource -> m Spaces
newSpaces region cs = do
    manager <- liftIO getGlobalManager
    (accessKey, secretKey) <- liftIO $ source cs
    return Spaces { .. }
  where
    source (Explicit ak sk)              = return (ak, sk)
    source (InEnv (Just (akEnv, skEnv))) =
        ensureKeys =<< (,) <$> lookupKey akEnv <*> lookupKey skEnv
    source (InEnv Nothing)               = do
        ak <- lookupKeys [ "AWS_ACCESS_KEY_ID"
                         , "SPACES_ACCESS_KEY_ID"
                         , "SPACES_ACCESS_KEY"
                         ]
        sk <- lookupKeys [ "AWS_SECRET_ACCESS_KEY"
                         , "SPACES_SECRET_ACCESS_KEY"
                         , "SPACES_SECRET_KEY"
                         ]
        ensureKeys (ak, sk)

    throwMissingKeys k = throwM . MissingKeys $ "Missing " <> k

    lookupKeys xs = asum <$> sequence (lookupEnv <$> xs)

    lookupKey = lookupEnv . T.unpack

    ensureKeys = \case
        (Just a, Just s) -> return (mkKey AccessKey a, mkKey SecretKey s)
        (Just _, _)      -> throwMissingKeys "secret key"
        (_, Just _)      -> throwMissingKeys "access key"
        (_, _)           -> throwMissingKeys "secret and access keys"

    mkKey f = f . C.pack

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
-- argument to a 'Network.HTTP.Client.Types.RequestBodyLBS'. Should you choose to
-- directly construct 'UploadObject', you must do this manually. 'multipartObject'
-- is more complicated, and takes care of chunking the request body, sending each
-- individual request, and completing the multipart request
--
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

-- | Initiate and complete a 'MultiPart' upload, using default 'UploadHeaders'.
-- If a 'SpacesException' is thrown while performing the transaction, an attempt
-- will be made to runSpaces a 'CancelMultipart' request, and the exception will be
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
        $ OtherError "multipartObject: Chunk size must be greater than/equal to 5MB"
    | otherwise = do
        session <- beginMultipart <&> (^. field' @"value" . field' @"session")
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
        go n = await >>= \case
            Nothing -> return ()
            Just v  -> do
                spaces <- ask
                etag <- liftIO
                    $ runSpaces spaces
                                (runAction NoMetadata
                                 $ UploadPart session n (RequestBodyLBS v))
                    <&> (^. field' @"value" . field' @"etag")
                yield etag >> go (n + 1)

    inChunks = loop 0 []
      where
        loop n chunk = await >>= maybe (yieldChunk chunk) go
          where
            go bs = bool (loop len newChunk)
                         (yieldChunk newChunk >> loop 0 [])
                         (size <= len)
              where
                len      = C.length bs + n

                newChunk = bs : chunk

        yieldChunk = yield . LB.fromChunks . reverse

-- | Get information about an 'Object' (does not retrieve the body of the object)
getObjectInfo
    :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse GetObjectInfo)
getObjectInfo bucket object = runAction KeepMetadata GetObjectInfo { .. }

-- | Get an 'Object' (retrieves the actual body of the object)
getObject :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse GetObject)
getObject bucket object = runAction KeepMetadata GetObject { .. }

-- | Copy an 'Object' from one 'Bucket' to another, or replace it
copyObject :: MonadSpaces m
           => Bucket -- ^ Source 'Bucket'
           -> Bucket -- ^ Destination 'Bucket'
           -> Object -- ^ Source 'Object'
           -> Object -- ^ Destination 'Object'
           -> MetadataDirective
           -> Maybe CannedACL
           -> m (SpacesResponse CopyObject)
copyObject srcBucket destBucket srcObject destObject metadataDirective acl =
    runAction KeepMetadata CopyObject { .. }

-- | Delete a single 'Object'
deleteObject
    :: MonadSpaces m => Bucket -> Object -> m (SpacesResponse DeleteObject)
deleteObject bucket object = runAction KeepMetadata DeleteObject { .. }

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

-- | List the 'Object's of a 'Bucket'
listBucket :: MonadSpaces m
           => Bucket
           -> Maybe Char -- ^ Delimiter
           -> Maybe Object -- ^ Marker
           -> Maybe Int -- ^ Maximum keys
           -> Maybe Text -- ^ Prefix used to group object keys
           -> m (SpacesResponse ListBucket)
listBucket bucket delimiter marker maxKeys prefix =
    runAction KeepMetadata ListBucket { .. }
