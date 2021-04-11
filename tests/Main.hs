{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Main where

import           Conduit                                     ( withSourceFile
                                                             )

import qualified Data.ByteString.Char8                       as C
import           Data.Function                               ( (&) )
import qualified Data.Sequence                               as S
import           Data.Time                                   ( UTCTime )
import           Data.Time.Format.ISO8601                    ( iso8601ParseM )

import           Network.DO.Spaces.Actions.GetBucketLocation
                 ( GetBucketLocation
                 , GetBucketLocationResponse(..)
                 )
import           Network.DO.Spaces.Actions.ListAllBuckets
                 ( ListAllBuckets
                 , ListAllBucketsResponse(..)
                 )
import           Network.DO.Spaces.Actions.ListBucket
                 ( ListBucket
                 , ListBucketResponse(..)
                 )
import           Network.DO.Spaces.Request
import           Network.DO.Spaces.Types
import           Network.HTTP.Client.Conduit                 ( Manager )
import           Network.HTTP.Client.TLS                     ( getGlobalManager
                                                             )

import           Test.Hspec
                 ( describe
                 , hspec
                 , it
                 , shouldBe
                 )

{- HLINT ignore "Redundant do" -}
main :: IO ()
main = sequence_ [ requests
                 , listAllBucketsResponse
                 , listBucketResponse
                 , bucketLocationResponse
                 ]

requests :: IO ()
requests = do
    mgr <- getGlobalManager
    spacesRequest <- newSpacesRequest (testBuilder mgr) testTime

    hspec $ do
        describe "Spaces requests" $ do
            it "Generates the canonical request"
                $ (spacesRequest & canonicalRequest) `shouldBe` canonRequest

            it "Generates the string to sign" $ do
                mkStringToSign spacesRequest `shouldBe` strToSign

            it "Generates the signature" $ do
                mkSignature spacesRequest strToSign `shouldBe` sig

            it "Generates the authorization" $ do
                mkAuthorization spacesRequest strToSign `shouldBe` auth
  where
    bodyHash     =
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

    canonRequest = Canonicalized
        $ C.intercalate "\n"
                        [ "GET"
                        , "/"
                        , ""
                        , "host:a-bucket.sgp1.digitaloceanspaces.com"
                        , "x-amz-content-sha256:" <> bodyHash
                        , "x-amz-date:20210404T214315Z"
                        , ""
                        , "host;x-amz-content-sha256;x-amz-date"
                        , bodyHash
                        ]

    strToSign    = StringToSign
        $ C.intercalate "\n"
                        [ "AWS4-HMAC-SHA256"
                        , "20210404T214315Z"
                        , "20210404/sgp1/s3/aws4_request"
                        , "266d2fb56a251205c42c7e0deb7d2e370574cf190f366ecf53179c27697c8e38"
                        ]

    sig          =
        Signature "3d0da77e916e588d05f0190f8c350eddb47337953897b1e0cfdb44075fd6b2b9"

    auth         = Authorization
        $ mconcat [ "AWS4-HMAC-SHA256 Credential="
                  , "II5JDQBAN3JYM4DNEB6C"
                  , "/"
                  , "20210404/sgp1/s3/aws4_request, "
                  , "SignedHeaders="
                  , "host;x-amz-content-sha256;x-amz-date, "
                  , "Signature="
                  , uncompute sig
                  ]

testTime :: UTCTime
testTime = read @UTCTime "2021-04-04 21:43:15 +0000"

testSpaces :: Manager -> Spaces
testSpaces = Spaces (AccessKey "II5JDQBAN3JYM4DNEB6C")
                    (SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY")
                    Singapore

testBuilder :: Manager -> SpacesRequestBuilder
testBuilder mgr = SpacesRequestBuilder
    { spaces      = testSpaces mgr
    , method      = Nothing
    , body        = Nothing
    , headers     = mempty
    , bucket      = Just testBucket
    , object      = Nothing
    , queryString = Nothing
    }

testBucket :: Bucket
testBucket = Bucket "a-bucket"

listAllBucketsResponse :: IO ()
listAllBucketsResponse = do
    allBuckets <- withSourceFile "./tests/data/list-all-buckets.xml"
                                 (consumeResponse @ListAllBuckets)
    bucketDate1 <- iso8601ParseM @_ @UTCTime "2017-06-23T18:37:48.157Z"
    bucketDate2 <- iso8601ParseM @_ @UTCTime "2017-06-23T18:37:48.157Z"

    hspec $ do
        describe "ListAllBuckets response" $ do
            it "parses ListAllBucketsResponse correctly" $ do
                allBuckets
                    `shouldBe` ListAllBucketsResponse
                    { owner   = Owner (ID 6174283) (ID 6174283)
                    , buckets =
                          S.fromList [ BucketInfo (Bucket "static-images")
                                                  bucketDate1
                                     , BucketInfo (Bucket "log-files")
                                                  bucketDate2
                                     ]
                    }

listBucketResponse :: IO ()
listBucketResponse = do
    bucketContents <- withSourceFile "./tests/data/list-bucket.xml"
                                     (consumeResponse @ListBucket)
    objectDate1 <- iso8601ParseM @_ @UTCTime "2017-07-13T18:40:46.777Z"
    objectDate2 <- iso8601ParseM @_ @UTCTime "2017-07-14T17:44:03.597Z"
    hspec $ do
        describe "ListBucket response" $ do
            it "parses ListBucketResponse correctly" $ do
                bucketContents
                    `shouldBe` ListBucketResponse
                    { bucket      = Bucket "static-images"
                    , prefix      = Nothing
                    , marker      = Nothing
                    , nextMarker  = Nothing
                    , maxKeys     = 1000
                    , isTruncated = False
                    , objects     =
                          S.fromList [ ObjectInfo
                                       { object       = Object "example.txt"
                                       , lastModified = objectDate1
                                       , etag         =
                                             "b3a92f49e7ae64acbf6b3e76f2040f5e"
                                       , size         = 14
                                       , owner        =
                                             Owner (ID 6174283) (ID 6174283)
                                       }
                                     , ObjectInfo
                                       { object       = Object "sammy.png"
                                       , lastModified = objectDate2
                                       , etag         =
                                             "fb08934ef619f205f272b0adfd6c018c"
                                       , size         = 35369
                                       , owner        =
                                             Owner (ID 6174283) (ID 6174283)
                                       }
                                     ]
                    }

bucketLocationResponse :: IO ()
bucketLocationResponse = do
    bucketContents <- withSourceFile "./tests/data/bucket-location.xml"
                                     (consumeResponse @GetBucketLocation)
    hspec $ do
        describe "GetBucketLocation response" $ do
            it "parses GetBucketLocationResponse correctly" $ do
                bucketContents
                    `shouldBe` GetBucketLocationResponse
                    { locationConstraint = NewYork }
