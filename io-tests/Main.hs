{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- WARNING
-- Run at your own risk!
--
-- Running the tests in this module requires an active Digital Ocean spaces
-- subscription, and may incur any of the following:
--
--      * charges billed to your DO account
--      * the application of rate-limiting or other limits to your account
--      * overage in data transfer or concurrent active spaces, resulting in
--        charges or penalties
--      * the creation of extraneous spaces/buckets in your subscription
--      * data loss or corruption
--
-- To run:
--      * copy ./creds.conf.skel to ./creds.conf
--      * replace the dummy values with your active Spaces access and
--        secret keys, a region slug, and the name of an existing bucket
--        to use for certain object CRUD operations
--      * do not add any additional fields, whitespace, quotes, etc... or
--        parsing the credentials file will fail
--      * run @cabal test@ with @-f test-io@
--
-- The tests will create buckets and attempt to delete them. In the event
-- that bucket deletion fails, you will have to clean them up manually.
-- Similarly, the default bucket provided in the "bucket" field of the
-- credentials file will be used to test certain object CRUD functionality.
-- Although the tests will attempt to delete any objects created, you may
-- need to delete them manually if this fails
--
module Main where

import           Conduit
                 ( (.|)
                 , runConduitRes
                 , sinkList
                 , sourceFile
                 , sourceLazy
                 )

import           Control.Exception          ( bracket, throwIO )
import           Control.Monad              ( void )
import           Control.Monad.Catch        ( MonadCatch(catch)
                                            , MonadThrow(throwM)
                                            )
import           Control.Monad.IO.Class     ( MonadIO(liftIO) )

import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as LBC
import           Data.Coerce                ( coerce )
import qualified Data.Conduit.Binary        as CB
import           Data.Generics.Labels       ()
import qualified Data.Text                  as T
import           Data.Text                  ( Text )
import qualified Data.Text.Encoding         as T
import           Data.Time.Clock.POSIX      ( getPOSIXTime )

import           Lens.Micro

import           Network.DO.Spaces
import           Network.DO.Spaces.Utils    ( slugToRegion )
import qualified Network.HTTP.Types         as H

import           System.Time.Extra          ( sleep )

import           Test.Hspec

main :: IO ()
main = sequence_ [ bucketCreateDelete
                 , objectCreateDelete
                 , objectActions
                 , multipart
                 ]

objectActions :: IO ()
objectActions = do
    (bucket, spaces) <- readConf
    hspec . around withTestObject $ do
        describe "Network.DO.Spaces.Actions.GetObjectInfo"
            . it "retrieves object information"
            $ \object -> do
                info <- retry404 20 . runSpaces spaces
                    $ getObjectInfo bucket object
                (info ^. #result . #contentType) `shouldBe` "text/plain"
                (info ^. #result . #contentLength) `shouldBe` 18

        describe "Network.DO.Spaces.Actions.CopyObject"
            . it "copies an existing object to a new object"
            $ \object -> do
                destObject <- nameWithEpoch mkObject "test-object-copy-"
                copied <- runSpaces spaces
                    $ copyObjectWithin bucket object destObject
                getStatus copied `shouldBe` Just 200
                deleted <- retry404 20 . runSpaces spaces
                    $ deleteObject bucket destObject
                getStatus deleted `shouldBe` Just 204

        describe "Network.DO.Spaces.Actions.GetObject"
            . it "retrieves object data"
            $ \object -> do
                gotten <- runSpaces spaces $ getObject bucket object
                (gotten ^. #result . #objectData)
                    `shouldBe` "hello from haskell"
  where
    withTestObject = bracket uploadTestObject deleteTestObject

    uploadTestObject = do
        (bucket, spaces) <- readConf
        object <- nameWithEpoch mkObject "test-object-"
        void . runSpaces spaces
            $ uploadObject (Just "text/plain") bucket object body
        return object
      where
        body = sourceLazy "hello from haskell"

    deleteTestObject object = do
        (bucket, spaces) <- readConf
        void . runSpaces spaces $ deleteObject bucket object

multipart :: IO ()
multipart = do
    (bucket, spaces) <- readConf
    object <- nameWithEpoch mkObject "test-multipart-"
    hspec
        . after_ (cleanup spaces bucket object)
        . describe "Network.DO.Spaces.multipartObject"
        . it "uploads a multipart object"
        $ do
            mp <- runSpaces spaces
                $ multipartObject Nothing bucket object 5242880 body
            getStatus mp `shouldBe` Just 200
            (mp ^. #result . #object) `shouldBe` object
  where
    cleanup spaces bucket object =
        void . retry404 20 . runSpaces spaces $ deleteObject bucket object

    body = sourceLazy $ LBC.replicate 10485760 'f'

objectCreateDelete :: IO ()
objectCreateDelete = do
    (bucket, spaces) <- readConf
    object <- nameWithEpoch mkObject "test-object-"
    hspec
        . context "Network.DO.Spaces.Actions Object CRUD"
        . it "creates, reads, and deletes a new Object"
        $ do
            created
                <- runSpaces spaces $ uploadObject Nothing bucket object body
            getStatus created `shouldBe` Just 200

            deleted
                <- retry404 20 . runSpaces spaces $ deleteObject bucket object
            getStatus deleted `shouldBe` Just 204
            (deleted ^. #result) `shouldBe` ()

            pending
  where
    body = sourceLazy "hello from haskell"

bucketActions :: IO ()
bucketActions = do
    (bucket, spaces) <- readConf
    hspec $ do
        describe "Network.DO.Spaces.Actions.GetBucketLocation"
            . it "retrieves a bucket's location"
            $ do
                location <- runSpaces spaces $ getBucketLocation bucket
                getStatus location `shouldBe` Just 200
                (location ^. #result . #locationConstraint)
                    `shouldBe` (spaces ^. #region)

        describe "Network.DO.Spaces.Actions.ListAllBuckets"
            . it "lists account owner's buckets"
            $ do
                allBuckets <- runSpaces spaces listAllBuckets
                (allBuckets ^.. #result . #buckets . each . #name)
                    `shouldContain` [ bucket ]

bucketCreateDelete :: IO ()
bucketCreateDelete = do
    (_, spaces) <- readConf
    bucket <- nameWithEpoch mkBucket "do-spaces-test-"
    hspec
        . context "Network.DO.Spaces.Actions Bucket CRUD"
        . it "creates, reads, and deletes a new bucket"
        $ do
            created <- runSpaces spaces $ createBucket bucket Nothing Nothing
            getStatus created `shouldBe` Just 200
            (created ^. #result) `shouldBe` ()

            location
                <- retry404 20 . runSpaces spaces $ getBucketLocation bucket
            getStatus location `shouldBe` Just 200
            (location ^. #result . #locationConstraint)
                `shouldBe` (spaces ^. #region)

            deleted <- retry404 20 . runSpaces spaces $ deleteBucket bucket
            getStatus deleted `shouldBe` Just 204
            (deleted ^. #result) `shouldBe` ()

-- A crude (yet effective) mechanism to ensure that 404s are retried (up to a
-- limit), necessary when creating a new Bucket for testing
retry404 :: (MonadCatch m, MonadIO m)
         => Int
         -> m (SpacesResponse a)
         -> m (SpacesResponse a)
retry404 maxRetries action = loop 0
  where
    loop n = catch @_ @ClientException action (catch404 n)

    catch404 n e@(HTTPStatus s _)
        | H.statusCode s == 404, n < maxRetries = liftIO (sleep 1)
            >> loop (n + 1)
        | otherwise = throwM e

    catch404 _ e                  = throwM e

-- Makes a Bucket or Object name by appending the current epoch time to a base name;
-- this provides some mechanism to avoid name clashes when creating new buckets,
-- which must be unique across all users within a given region. It also helps ensure
-- that new Objects created during testing won't overwrite existing ones in the
-- default Bucket provided in the credentials configuration
nameWithEpoch :: MonadIO m => (Text -> m a) -> Text -> m a
nameWithEpoch f base = f . (base <>) . T.pack . show @Integer . round
    =<< liftIO getPOSIXTime

readConf :: IO (Bucket, Spaces)
readConf = do
    contents <- runConduitRes
        $ sourceFile "./io-tests/creds.conf" .| CB.lines .| sinkList
    case getVal <$> contents of
        [ a, s, r, b ] -> do
            let access = coerce a
                secret = coerce s
            region <- slugToRegion $ T.decodeUtf8 r
            bucket <- mkBucket $ T.decodeUtf8 b
            spaces <- newSpaces region (Explicit access secret)
            return (bucket, spaces)
        _              -> throwIO . userError
            $ mconcat [ "io-tests: Credentials must consist of "
                      , "exactly four lines, in the order "
                      , "'access', 'secret', 'region', 'bucket'"
                      , "with each key separated from its "
                      , "value with '=', without whitespace "
                      , "See ./creds.conf.skel"
                      ]
  where
    getVal = snd . C.splitAt 7

getStatus :: SpacesResponse a -> Maybe Int
getStatus = (^? #metadata . _Just . #status . to H.statusCode)
