{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Main where

import           Conduit                                  ( withSourceFile )

import qualified Data.ByteString.Char8                    as C
import           Data.Function                            ( (&) )
import qualified Data.Sequence                            as S
import           Data.Time                                ( UTCTime )
import           Data.Time.Format.ISO8601                 ( iso8601ParseM )

import           Network.DO.Spaces.Actions.ListAllBuckets
                 ( ListAllBuckets
                 , ListAllBucketsResponse(..)
                 )
import           Network.DO.Spaces.Request
import           Network.DO.Spaces.Types
import           Network.HTTP.Client.Conduit              ( Manager )
import           Network.HTTP.Client.TLS                  ( getGlobalManager )

import           Test.Hspec
                 ( describe
                 , hspec
                 , it
                 , shouldBe
                 )

{- HLINT ignore "Redundant do" -}
main :: IO ()
main = requests >> listAllBucketsResponse

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
        $ C.unlines [ "GET"
                    , "/"
                    , ""
                    , "content-length:0"
                    , "host:a-bucket.sgp1.digitaloceanspaces.com"
                    , "x-amz-content-sha256:" <> bodyHash
                    , ""
                    , "content-length;host;x-amz-content-sha256"
                    , bodyHash
                    ]

    strToSign    = StringToSign
        $ C.unlines [ "AWS4-HMAC-SHA256"
                    , "20210404T214315Z"
                    , "20210404/sgp1/s3/aws4_request"
                    , "d121ec555d43524fda5d6daea0b68654cd2bbcb75c4962c5b1eef8887bf732df"
                    ]

    sig          =
        Signature "1d0b8f8092719a53f9fafd9695fffa85057e3bc6f02f33430e161ec388b1beee"

    auth         = Authorization
        $ mconcat [ "AWS4-HMAC-SHA256 Credential="
                  , "II5JDQBAN3JYM4DNEB6C"
                  , "/"
                  , "20210404/sgp1/s3/aws4_request, "
                  , "SignedHeaders="
                  , "content-length;host;x-amz-content-sha256, "
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
