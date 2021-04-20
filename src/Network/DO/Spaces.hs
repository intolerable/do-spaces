{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces
    ( send
    , newSpaces
      -- * Convenience actions
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
import qualified Data.Text                   as T
import           Data.Text                   ( Text )

import           Network.DO.Spaces.Actions
import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils     ( defaultUploadHeaders )
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import           Network.HTTP.Client.TLS     ( getGlobalManager )
import           Network.Mime                ( MimeType )

import           System.Environment          ( lookupEnv )

-- | Perform a transaction using your 'Spaces' client configuration
send :: Spaces -> SpacesT m a -> m a
send = flip runSpacesT

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

-- | Upload an 'Object' within a single request
uploadObject :: MonadSpaces m
             => Maybe MimeType
             -> Bucket
             -> Object
             -> BodyBS m
             -> m UploadObjectResponse
uploadObject contentType bucket object rbody = do
    body <- RequestBodyLBS <$> runConduit (rbody .| sinkLbs)
    runAction UploadObject { optionalHeaders = defaultUploadHeaders, .. }

-- | Initiate and complete a 'MultiPart' upload, using default 'UploadHeaders'.
-- If a 'SpacesException' is thrown while performing the transaction, an attempt
-- will be made to send a 'CancelMultipart' request, and the exception will be
-- rethrown
multipartObject
    :: MonadSpaces m
    => Maybe MimeType
    -> Bucket
    -> Object
    -> Int
    -> BodyBS m
    -> m CompleteMultipartResponse
multipartObject contentType bucket object size body
    | size < 5242880 = throwM
        $ OtherError "multipartObject: Chunk size must be greater than/equal to 5MB"
    | otherwise = do
        BeginMultipartResponse session <- beginMultipart
        catch @_ @SpacesException (run session) $ \e -> do
            void . runAction $ CancelMultipart session
            throwM e
  where
    run session = completeMultipart session
        =<< runConduit (body .| inChunks .| putPart session .| consume)

    completeMultipart session tags = runAction
        $ CompleteMultipart session (zip [ 1 .. ] tags)

    beginMultipart = runAction BeginMultipart
                               { optionalHeaders = defaultUploadHeaders, .. }

    putPart session = go 1
      where
        go n = await >>= \case
            Nothing -> return ()
            Just v  -> do
                spaces <- ask
                UploadPartResponse { etag } <- liftIO
                    $ send spaces
                           (runAction
                            $ UploadPart session n (RequestBodyLBS v))
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
getObjectInfo :: MonadSpaces m => Bucket -> Object -> m GetObjectInfoResponse
getObjectInfo bucket object = runAction GetObjectInfo { .. }

-- | Get an 'Object' (retrieves the actual body of the object)
getObject :: MonadSpaces m => Bucket -> Object -> m GetObjectResponse
getObject bucket object = runAction GetObject { .. }

-- | Copy an 'Object' from one 'Bucket' to another, or replace it
copyObject :: MonadSpaces m
           => Bucket -- ^ Source 'Bucket'
           -> Bucket -- ^ Destination 'Bucket'
           -> Object -- ^ Source 'Object'
           -> Object -- ^ Destination 'Object'
           -> MetadataDirective
           -> Maybe CannedACL
           -> m CopyObjectResponse
copyObject srcBucket destBucket srcObject destObject metadataDirective acl =
    runAction CopyObject { .. }

-- | Delete a single 'Object'
deleteObject :: MonadSpaces m => Bucket -> Object -> m DeleteObjectResponse
deleteObject bucket object = runAction DeleteObject { .. }

-- | Create a new 'Bucket'
createBucket :: MonadSpaces m
             => Bucket
             -> Maybe Region -- ^ Overrides the 'Region' in your 'Spaces'
                             -- configuration
             -> Maybe CannedACL
             -> m CreateBucketResponse
createBucket bucket region acl = runAction CreateBucket { .. }

-- | Delete a 'Bucket'
deleteBucket :: MonadSpaces m => Bucket -> m DeleteBucketResponse
deleteBucket bucket = runAction DeleteBucket { .. }

-- | Get the location ('Region') of a 'Bucket'
getBucketLocation :: MonadSpaces m => Bucket -> m GetBucketLocationResponse
getBucketLocation bucket = runAction GetBucketLocation { .. }

-- | List every 'Bucket' associated with your Spaces account
listAllBuckets :: MonadSpaces m => m ListAllBucketsResponse
listAllBuckets = runAction ListAllBuckets

-- | List the 'Object's of a 'Bucket'
listBucket :: MonadSpaces m
           => Bucket
           -> Maybe Char -- ^ Delimiter
           -> Maybe Object -- ^ Marker
           -> Maybe Int -- ^ Maximum keys
           -> Maybe Text -- ^ Prefix used to group object keys
           -> m ListBucketResponse
listBucket bucket delimiter marker maxKeys prefix =
    runAction ListBucket { .. }

