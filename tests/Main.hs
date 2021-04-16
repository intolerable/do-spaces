{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Main where

import           Conduit                   ( sourceLazy, withSourceFile )

import qualified Data.ByteString.Char8     as C
import qualified Data.CaseInsensitive      as CI
import           Data.Function             ( (&) )
import qualified Data.Sequence             as S
import           Data.Time                 ( UTCTime )
import           Data.Time.Format.ISO8601  ( iso8601ParseM )

import           Network.DO.Spaces         ( newSpaces )
import           Network.DO.Spaces.Actions
                 ( CopyObject(..)
                 , CopyObjectResponse(..)
                 , GetBucketLocation
                 , GetBucketLocationResponse(..)
                 , GetObjectInfo
                 , ListAllBuckets
                 , ListAllBucketsResponse(..)
                 , ListBucket(..)
                 , ListBucketResponse(..)
                 , MetadataDirective(Copy)
                 , parseErrorResponse
                 )
import           Network.DO.Spaces.Request
import           Network.DO.Spaces.Types
import           Network.HTTP.Types        ( mkStatus )

import           Test.Hspec
                 ( describe
                 , hspec
                 , it
                 , shouldBe
                 , shouldThrow
                 )

main :: IO ()
main = sequence_ [ requests
                 , errorResponse
                 , listAllBucketsResponse
                 , listBucket
                 , bucketLocationResponse
                 , objectInfoResponse
                 , copyObject
                 ]

requests :: IO ()
requests = do
    sp <- testSpaces
    spacesRequest <- newSpacesRequest (testBuilder sp) testTime

    hspec . describe "Spaces requests" $ do
        it "Generates the canonical request"
            $ (spacesRequest & canonicalRequest) `shouldBe` canonRequest

        it "Generates the string to sign"
            $ mkStringToSign spacesRequest `shouldBe` strToSign

        it "Generates the signature"
            $ mkSignature spacesRequest strToSign `shouldBe` sig

        it "Generates the authorization"
            $ mkAuthorization spacesRequest strToSign `shouldBe` auth
  where
    bodyHash           =
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

    canonRequest       = Canonicalized
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

    strToSign          = StringToSign
        $ C.intercalate "\n"
                        [ "AWS4-HMAC-SHA256"
                        , "20210404T214315Z"
                        , "20210404/sgp1/s3/aws4_request"
                        , "266d2fb56a251205c42c7e0deb7d2e370574cf190f366ecf53179c27697c8e38"
                        ]

    sig                =
        Signature "3d0da77e916e588d05f0190f8c350eddb47337953897b1e0cfdb44075fd6b2b9"

    auth               = Authorization
        $ mconcat [ "AWS4-HMAC-SHA256 Credential="
                  , "II5JDQBAN3JYM4DNEB6C"
                  , "/"
                  , "20210404/sgp1/s3/aws4_request, "
                  , "SignedHeaders="
                  , "host;x-amz-content-sha256;x-amz-date, "
                  , "Signature="
                  , uncompute sig
                  ]

    testTime           = read @UTCTime "2021-04-04 21:43:15 +0000"

    testBuilder spaces = SpacesRequestBuilder
        { spaces
        , method         = Nothing
        , body           = Nothing
        , headers        = mempty
        , bucket         = Just testBucket
        , object         = Nothing
        , queryString    = Nothing
        , overrideRegion = Nothing
        }

    testBucket         = Bucket "a-bucket"

testSpaces :: IO Spaces
testSpaces =
    newSpaces Singapore
              (Explicit (AccessKey "II5JDQBAN3JYM4DNEB6C")
                        (SecretKey "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"))

errorResponse :: IO ()
errorResponse = do
    apiEx <- withSourceFile "./tests/data/error-response.xml" $ \body -> do
        let headers = mempty
            raw     = RawResponse { .. }
        parseErrorResponse status raw

    hspec
        . describe "APIException parsing"
        . it "parses APIException correctly"
        $ apiEx
        `shouldBe` APIException
        { code      = "SignatureDoesNotMatch"
        , requestID = "tx000012a832c-nyc3"
        , hostID    = "71f0230-nyc3a-nyc"
        , status
        }
  where
    status = mkStatus 403 ""

listAllBucketsResponse :: IO ()
listAllBucketsResponse = do
    sp <- testSpaces
    allBuckets
        <- withSourceFile "./tests/data/list-all-buckets.xml" $ \body -> do
            let headers = mempty
                raw     = RawResponse { .. }
            runSpacesT (consumeResponse @_ @ListAllBuckets raw) sp
    bucketDate1 <- iso8601ParseM @_ @UTCTime "2017-06-23T18:37:48.157Z"
    bucketDate2 <- iso8601ParseM @_ @UTCTime "2017-06-23T18:37:48.157Z"

    hspec
        . describe "ListAllBuckets response"
        . it "parses ListAllBucketsResponse correctly"
        $ allBuckets
        `shouldBe` ListAllBucketsResponse
        { owner   = Owner (OwnerID 6174283) (OwnerID 6174283)
        , buckets =
              S.fromList [ BucketInfo (Bucket "static-images") bucketDate1
                         , BucketInfo (Bucket "log-files") bucketDate2
                         ]
        }

listBucket :: IO ()
listBucket = do
    sp <- testSpaces
    bucketContents
        <- withSourceFile "./tests/data/list-bucket.xml" $ \body -> do
            let headers = mempty
                raw     = RawResponse { .. }
            runSpacesT (consumeResponse @_ @ListBucket raw) sp
    objectDate1 <- iso8601ParseM @_ @UTCTime "2017-07-13T18:40:46.777Z"
    objectDate2 <- iso8601ParseM @_ @UTCTime "2017-07-14T17:44:03.597Z"

    hspec . describe "ListBucket response" $ do
        it "parses ListBucketResponse correctly"
            $ bucketContents `shouldBe` listBucketResp objectDate1 objectDate2

        it "ensures maxKeys is within the correct range" $ do
            let badReq  = listBucketReq $ Just (-1)
                badReq2 = listBucketReq $ Just 1001
            runSpacesT (buildRequest badReq) sp
                `shouldThrow` (InvalidRequest msg ==)
            runSpacesT (buildRequest badReq2) sp
                `shouldThrow` (InvalidRequest msg ==)
  where
    listBucketReq maxKeys = ListBucket
        { bucket    = Bucket "some-bucket"
        , delimiter = Nothing
        , marker    = Nothing
        , prefix    = Nothing
        , maxKeys
        }

    msg                   = "ListBucket: maxKeys must be >= 0 && <= 1000"

    listBucketResp d1 d2 = ListBucketResponse
        { bucket      = Bucket "static-images"
        , prefix      = Nothing
        , marker      = Nothing
        , nextMarker  = Nothing
        , maxKeys     = 1000
        , isTruncated = False
        , objects     =
              S.fromList [ ObjectInfo
                           { object       = Object "example.txt"
                           , lastModified = d1
                           , etag         = "b3a92f49e7ae64acbf6b3e76f2040f5e"
                           , size         = 14
                           , owner        =
                                 Owner (OwnerID 6174283) (OwnerID 6174283)
                           }
                         , ObjectInfo
                           { object       = Object "sammy.png"
                           , lastModified = d2
                           , etag         = "fb08934ef619f205f272b0adfd6c018c"
                           , size         = 35369
                           , owner        =
                                 Owner (OwnerID 6174283) (OwnerID 6174283)
                           }
                         ]
        }

bucketLocationResponse :: IO ()
bucketLocationResponse = do
    sp <- testSpaces
    bucketContents
        <- withSourceFile "./tests/data/bucket-location.xml" $ \body -> do
            let headers = mempty
                raw     = RawResponse { .. }
            runSpacesT (consumeResponse @_ @GetBucketLocation raw) sp

    hspec
        . describe "GetBucketLocation response"
        . it "parses GetBucketLocationResponse correctly"
        $ bucketContents
        `shouldBe` GetBucketLocationResponse { locationConstraint = NewYork }

objectInfoResponse :: IO ()
objectInfoResponse = do
    sp <- testSpaces
    let body    = sourceLazy mempty
        headers = [ (CI.mk "Content-Type", "text/plain")
                  , (CI.mk "Content-Length", "14")
                  , (CI.mk "Etag", "b3a92f49e7ae64acbf6b3e76f2040f5e")
                  , (CI.mk "Last-Modified", "Thu, 13 Jul 2017 18:40:46 GMT")
                  ]
        raw     = RawResponse { .. }
    objectInfo <- runSpacesT (consumeResponse @_ @GetObjectInfo raw) sp

    hspec
        . describe "GetObjectInfo response"
        . it "parses GetObjectInfo response headers correctly"
        $ objectInfo
        `shouldBe` ObjectMetadata
        { contentLength = 14
        , contentType   = "text/plain"
        , etag          = "b3a92f49e7ae64acbf6b3e76f2040f5e"
        , lastModified  = testTime
        }
  where
    testTime = read @UTCTime "2017-07-13 18:40:46 +0000"

copyObject :: IO ()
copyObject = do
    sp <- testSpaces
    copyObjectDate <- iso8601ParseM @_ @UTCTime "2017-07-10T20:22:54.167Z"
    copyObjectResp
        <- withSourceFile "./tests/data/copy-object.xml" $ \body -> do
            let headers = mempty
                raw     = RawResponse { .. }
            runSpacesT (consumeResponse @_ @CopyObject raw) sp

    hspec . describe "CopyObject request/response" $ do
        it "parses CopyObjectResponse correctly"
            $ copyObjectResp
            `shouldBe` CopyObjectResponse
            { lastModified = copyObjectDate
            , etag         = "7967bfe102f83fb5fc7e5a02bf05e8fc"
            }

        it "ensures the correct metadataDirective is provided"
            $ runSpacesT (buildRequest badReq) sp
            `shouldThrow` (InvalidRequest msg ==)
  where
    badReq    = CopyObject
        { srcBucket
        , srcObject
        , destBucket        = srcBucket
        , destObject        = srcObject
        , metadataDirective = Copy
        , acl               = Nothing
        }

    srcObject = Object "some-object"

    srcBucket = Bucket "some-bucket"

    msg       = mconcat [ "CopyObject: "
                        , "Object cannot be copied to itself unless "
                        , "REPLACE directive is specified"
                        ]
