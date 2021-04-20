{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module Network.DO.Spaces ( send, newSpaces, multipart, upload ) where

import           Conduit
                 ( (.|)
                 , await
                 , runConduit
                 , yield
                 )

import           Control.Exception           ( throwIO )
import           Control.Monad.Catch         ( MonadThrow(throwM) )
import           Control.Monad.IO.Class      ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class  ( ask )

import           Data.Bool                   ( bool )
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import           Data.Conduit.Binary         ( sinkLbs )
import           Data.Conduit.List           ( consume )
import           Data.Foldable               ( asum )
import qualified Data.Text                   as T

import           Network.DO.Spaces.Actions
import           Network.DO.Spaces.Types
import           Network.DO.Spaces.Utils     ( defaultUploadHeaders )
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import           Network.HTTP.Client.TLS     ( getGlobalManager )
import           Network.Mime                ( MimeType )

import           System.Environment          ( lookupEnv )

-- | Perform a transaction using your 'Spaces' client configuration
send :: Spaces -> SpacesT m a -> m a
send sp x = runSpacesT x sp

-- | Create a new 'Spaces' from your credentials and a 'Region'
newSpaces :: Region -> CredentialSource -> IO Spaces
newSpaces region cs = do
    manager <- getGlobalManager
    (accessKey, secretKey) <- source cs
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

    throwMissingKeys k = throwIO . MissingKeys $ "Missing " <> k

    lookupKeys xs = asum <$> sequence (lookupEnv <$> xs)

    lookupKey = lookupEnv . T.unpack

    ensureKeys = \case
        (Just a, Just s) -> return (mkKey AccessKey a, mkKey SecretKey s)
        (Just _, _)      -> throwMissingKeys "secret key"
        (_, Just _)      -> throwMissingKeys "access key"
        (_, _)           -> throwMissingKeys "secret and access keys"

    mkKey f = f . C.pack

-- | Upload an 'Object' within a single request
upload :: MonadSpaces m
       => Maybe MimeType
       -> Bucket
       -> Object
       -> BodyBS m
       -> m UploadObjectResponse
upload mt bucket object body = do
    rbody <- RequestBodyLBS <$> runConduit (body .| sinkLbs)
    runAction $ UploadObject bucket object rbody defaultUploadHeaders mt

    -- runAction $ UploadObject bucket object defaultUploadHeaders mt
-- | Initiate and complete a 'MultiPart' upload, using default 'UploadHeaders'
multipart :: MonadSpaces m
          => Maybe MimeType
          -> Bucket
          -> Object
          -> Int
          -> BodyBS m
          -> m CompleteMultipartResponse
multipart mt bucket object size body
    | size < 5242880 = throwM
        $ OtherError "multipart: Chunk size must be greater than/equal to 5MB"
    | otherwise = do
        BeginMultipartResponse session <- beginMultipart
        completeMultipart session
            =<< runConduit (body .| inChunks .| putPart session .| consume)
  where
    completeMultipart session tags = runAction
        $ CompleteMultipart session (zip [ 1 .. ] tags)

    beginMultipart =
        runAction $ BeginMultipart bucket object defaultUploadHeaders mt

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
