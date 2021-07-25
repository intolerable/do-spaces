{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- __WARNING__
-- Run at your own risk!
--
-- Running the tests in this module requires an active Digital Ocean spaces
-- subscription, and may incur any of the following:
--
--      * charges billed to your DO account
--      * the application of rate-limiting or other limits to your account
--      * overage in data transfer or concurrent active spaces, resulting in
--        charges or penalties
--      * the creation of extraneous spaces\/buckets in your subscription
--      * data loss or corruption
--
-- The author of this package does not take responsibility for any of the above
-- or any other damages not is explicitly mentioned here.
--
-- Before running, make sure that the 'Discover' credentials source will
-- succeed. In addition, make sure to export the @SPACES_DEFAULT_BUCKET@
-- environment variable. This should correspond to a bucket name that will
-- be used to run some of the tests
--
-- The tests will create buckets and attempt to delete them. In the event
-- that bucket deletion fails, you will have to clean them up manually.
-- Similarly, the default bucket as exported by @SPACES_DEFAULT_BUCKET@
-- will be used to test certain object CRUD functionality. Although the tests
-- will attempt to delete any objects created, you may need to delete them
-- manually if cleanup fails
--
module Main where

import           Conduit

import           Control.Exception          ( bracket, throwIO )
import           Control.Monad              ( void )
import           Control.Monad.Catch        ( MonadCatch(catch) )

import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Generics.Labels       ()
import qualified Data.Text                  as T
import           Data.Text                  ( Text )
import           Data.Time.Clock.POSIX      ( getPOSIXTime )

import           Lens.Micro
import           Lens.Micro.GHC             ()

import           Network.DO.Spaces
import qualified Network.HTTP.Types         as H

import           System.Environment         ( lookupEnv )
import           System.Time.Extra          ( sleep )

import           Test.Hspec

main :: IO ()
main = hspec $ sequence_ [ objectActionSpec, multipartSpec, bucketSpec ]

{- HLINT ignore "Redundant do" -}
objectActionSpec :: Spec
objectActionSpec = around withTestObject $ do
    describe "Network.DO.Spaces.Actions.GetObjectInfo" $ do
        it "retrieves object information" $ \(bucket, spaces, object) -> do
            info <- retry404 20 . runSpaces spaces
                $ getObjectInfo bucket object
            info ^. (#result . #contentType) `shouldBe` "text/plain"
            info ^. (#result . #contentLength) `shouldBe` 18

    describe "Network.DO.Spaces.Actions.CopyObject" $ do
        it "copies an existing object to a new object"
            $ \(bucket, spaces, object) -> do
                destObject <- nameWithEpoch mkObject "test-object-copy-"
                copied <- runSpaces spaces
                    $ copyObjectWithin bucket object destObject
                getStatus copied `shouldBe` Just 200
                deleted <- retry404 20 . runSpaces spaces
                    $ deleteObject bucket destObject
                getStatus deleted `shouldBe` Just 204

    describe "Network.DO.Spaces.Actions.GetObject" $ do
        it "retrieves object data" $ \(bucket, spaces, object) -> do
            gotten <- runSpaces spaces $ getObject bucket object
            gotten ^. (#result . #objectData) `shouldBe` "hello from haskell"
  where
    withTestObject = bracket uploadTestObject deleteTestObject

    uploadTestObject = do
        (bucket, spaces) <- getCreds
        object <- nameWithEpoch mkObject "test-object-"
        void . runSpaces spaces
            $ uploadObject (Just "text/plain") bucket object body
        pure (bucket, spaces, object)
      where
        body = sourceLazy "hello from haskell"

    deleteTestObject (bucket, spaces, object) = do
        void . runSpaces spaces $ deleteObject bucket object

multipartSpec :: Spec
multipartSpec = do
    around withMultipart $ do
        describe "Network.DO.Spaces.multipartObject" $ do
            it "uploads a multipart object" $ \(bucket, spaces, object) -> do
                mp <- runSpaces spaces
                    $ multipartObject Nothing bucket object 5242880 body
                getStatus mp `shouldBe` Just 200
                mp ^. (#result . #object) `shouldBe` object
  where
    withMultipart = bracket uploadTestObject cleanup

    uploadTestObject = do
        (bucket, spaces) <- getCreds
        object <- nameWithEpoch mkObject "test-multipart-"
        pure (bucket, spaces, object)

    cleanup (bucket, spaces, object) =
        void . retry404 20 . runSpaces spaces $ deleteObject bucket object

    body = sourceLazy $ LC.replicate 10485760 'f'

bucketSpec :: Spec
bucketSpec = beforeAll getCreds $ do
    describe "Network.DO.Spaces.Actions.GetBucketLocation" $ do
        it "retrieves a bucket's location" $ \(bucket, spaces) -> do
            location <- runSpaces spaces $ getBucketLocation bucket
            getStatus location `shouldBe` Just 200
            location
                ^. (#result . #locationConstraint)
                `shouldBe` --
                spaces
                ^. #region

    describe "Network.DO.Spaces.Actions.ListAllBuckets" $ do
        it "lists account owner's buckets" $ \(bucket, spaces) -> do
            allBuckets <- runSpaces spaces listAllBuckets
            allBuckets
                ^.. (#result . #buckets . each . #name)
                `shouldContain` [ bucket ]

    context "Network.DO.Spaces.Actions Bucket CRUD" $ do
        it "creates, reads, and deletes a new bucket" $ \(_, spaces) -> do
            bucket <- nameWithEpoch mkBucket "do-spaces-test-"
            created <- runSpaces spaces $ createBucket bucket Nothing Nothing
            getStatus created `shouldBe` Just 200
            created ^. #result `shouldBe` ()
            deleted <- retry404 20 . runSpaces spaces $ deleteBucket bucket
            getStatus deleted `shouldBe` Just 204
            deleted ^. #result `shouldBe` ()

-- A crude mechanism to ensure that 404s are retried (up to a limit), necessary
-- when creating a new @Bucket@ for testing
retry404 :: (MonadCatch m, MonadIO m)
         => Int
         -> m (SpacesResponse a)
         -> m (SpacesResponse a)
retry404 maxRetries action = loop 0
  where
    loop !n = catch @_ @ClientException action $ catch404 n

    catch404 !n e@(HTTPStatus s _)
        | H.statusCode s == 404, n < maxRetries = liftIO (sleep 1)
            >> loop (n + 1)
        | otherwise = throwM e

    catch404 _ e                   = throwM e

-- Makes a Bucket or Object name by appending the current epoch time to a base name;
-- this provides some mechanism to avoid name clashes when creating new buckets,
-- which must be unique across all users within a given region. It also helps ensure
-- that new Objects created during testing won't overwrite existing ones in the
-- default Bucket provided in the credentials configuration
nameWithEpoch :: MonadIO m => (Text -> m a) -> Text -> m a
nameWithEpoch f base = f . (base <>) . T.pack . show @Integer . round
    =<< liftIO getPOSIXTime

getCreds :: IO (Bucket, Spaces)
getCreds = lookupEnv "SPACES_DEFAULT_BUCKET" >>= \case
    Nothing -> throwIO
        $ userError "Please export SPACES_DEFAULT_BUCKET to run the tests"
    Just b  -> (,) <$> mkBucket (T.pack b) <*> newSpaces Discover

getStatus :: SpacesResponse a -> Maybe Int
getStatus = (^? #metadata . _Just . #status . to H.statusCode)
