{-# LANGUAGE BangPatterns #-}
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
--        secret keys and a region slug
--      * do not add any additional fields, whitespace, quotes, etc... or
--        parsing the credentials file will fail
--      * run @cabal test@ with @-f test-io@
--
-- The tests will create buckets and attempt to delete them. In the event
-- that bucket deletion fails, you will have to clean them up manually
--
module Main where

import           Conduit
                 ( (.|)
                 , MonadThrow(throwM)
                 , runConduitRes
                 , sinkList
                 , sourceFile
                 )

import           Control.Exception       ( throwIO )
import           Control.Monad.Catch     ( MonadCatch(catch) )
import           Control.Monad.IO.Class  ( MonadIO(liftIO) )

import qualified Data.ByteString.Char8   as C
import           Data.Coerce             ( coerce )
import qualified Data.Conduit.Binary     as CB
import           Data.Generics.Labels    ()
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Clock.POSIX   ( getPOSIXTime )

import           Lens.Micro

import           Network.DO.Spaces
import           Network.DO.Spaces.Types ( SpacesResponse )
import           Network.DO.Spaces.Utils ( slugToRegion )
import qualified Network.HTTP.Types      as H

import           System.Time.Extra       ( sleep )

import           Test.Hspec

main :: IO ()
main = bucketCrud

bucketCrud :: IO ()
bucketCrud = do
    sp <- readConf
    !bucket <- epochBucket
    hspec
        . context "Network.DO.Spaces.Actions Bucket CRUD"
        . it "creates, reads, and deletes a new bucket"
        $ do
            created <- runSpaces sp $ createBucket bucket Nothing Nothing
            (created ^? #metadata . _Just . #status . to H.statusCode)
                `shouldBe` Just 200
            (created ^. #value) `shouldBe` ()

            location <- retry404 20 . runSpaces sp $ getBucketLocation bucket
            (location ^? #metadata . _Just . #status . to H.statusCode)
                `shouldBe` Just 200
            (location ^. #value . #locationConstraint)
                `shouldBe` (sp ^. #region)

            deleted <- retry404 20 . runSpaces sp $ deleteBucket bucket
            (deleted ^? #metadata . _Just . #status . to H.statusCode)
                `shouldBe` Just 204
            (deleted ^. #value) `shouldBe` ()

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

-- Makes a Bucket by appending the current epoch time to a base name; this
-- provides some mechanism to avoid name clashes when creating new buckets,
-- which must be unique across all users within a given region
epochBucket :: IO Bucket
epochBucket =
    mkBucket . ("do-spaces-test-" <>) . T.pack . show @Integer . round
    =<< getPOSIXTime

readConf :: IO Spaces
readConf = do
    contents <- runConduitRes
        $ sourceFile "./io-tests/creds.conf" .| CB.lines .| sinkList
    case contents of
        [ a, s, r ] -> do
            let access = coerce $ getVal a
                secret = coerce $ getVal s
            region <- slugToRegion . T.decodeUtf8 $ getVal r
            newSpaces region (Explicit access secret)
        _           -> throwIO . userError
            $ mconcat [ "io-tests: Credentials must consist of "
                      , "exactly three lines, in the order "
                      , "'access', 'secret', 'region', "
                      , "with each key separated from its "
                      , "value with '='. "
                      , "See ./creds.conf.skel"
                      ]
  where
    getVal = snd . C.splitAt 7
