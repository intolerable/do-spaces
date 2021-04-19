{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Network.DO.Spaces.Actions.UploadMultipart
    ( BeginMultipart(..)
    , BeginMultipartResponse(..)
    , UploadPart(..)
    , UploadPartResponse(..)
    , ListParts(..)
    , Part(..)
    , ListPartsResponse(..)
    , UploadID
    , CancelMultipart(..)
    , MultipartSession(..)
    , CancelMultipartResponse
    , CompleteMultipart(..)
    , CompleteMultipartResponse(..)
    ) where

import           Control.Monad.Catch         ( MonadThrow(throwM) )
import           Control.Monad.Reader        ( MonadReader(ask) )
import           Control.Monad.Trans.Maybe   ( MaybeT(runMaybeT) )

import           Data.ByteString             ( ByteString )
import           Data.Generics.Product       ( HasField(field) )
import           Data.Sequence               ( Seq )
import qualified Data.Sequence               as S
import           Data.Text                   ( Text )
import           Data.Time                   ( UTCTime )

import           GHC.Generics                ( Generic )

import           Lens.Micro                  ( (^.), (^?) )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , ClientException(OtherError)
                 , ETag
                 , Method(POST, DELETE, PUT)
                 , MonadSpaces
                 , Object
                 , SpacesRequestBuilder(..)
                 , UploadHeaders
                 )
import           Network.DO.Spaces.Utils
                 ( bucketP
                 , etagP
                 , isTruncP
                 , lastModifiedP
                 , lookupHeader
                 , objectP
                 , quote
                 , readEtag
                 , renderUploadHeaders
                 , tshow
                 , xmlDocCursor
                 , xmlElemError
                 , xmlNum
                 )
import           Network.HTTP.Client.Conduit ( RequestBody(RequestBodyLBS) )
import qualified Network.HTTP.Types          as H

import qualified Text.XML                    as X
import qualified Text.XML.Cursor             as X
import           Text.XML.Cursor             ( ($/), (&/), (&|) )

-- | A single part of a multipart upload session. Returned when querying 'ListParts'
data Part = Part
    { partNumber   :: Int
    , lastModified :: UTCTime
    , etag         :: ETag
    , size         :: Int -- ^ Size in bytes
    }
    deriving ( Show, Eq, Generic )

data MultipartSession = MultipartSession
    { bucket   :: Bucket
    , object   :: Object
    , uploadID :: UploadID --
    }
    deriving ( Show, Eq, Generic )

-- | A unique ID assigned to a multipart upload session
type UploadID = Text

-- | Initiate a multipart upload session
data BeginMultipart = BeginMultipart
    { bucket          :: Bucket
    , object          :: Object
    , optionalHeaders :: UploadHeaders --
    }
    deriving ( Show, Eq, Generic )

newtype BeginMultipartResponse =
    BeginMultipartResponse { session :: MultipartSession }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m BeginMultipart where
    type (SpacesResponse BeginMultipart) = BeginMultipartResponse

    buildRequest BeginMultipart { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , object         = Just object
               , method         = Just POST
               , body           = Nothing
               , overrideRegion = Nothing
               , headers        = renderUploadHeaders optionalHeaders
               , queryString    = Just
                     $ H.toQuery [ ( "uploads" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        object <- objectP cursor
        bucket <- bucketP cursor
        uploadID <- X.force (xmlElemError "UploadId")
            $ cursor $/ X.laxElement "UploadId" &/ X.content
        return $ BeginMultipartResponse { session = MultipartSession { .. } }

data UploadPart = UploadPart
    { session :: MultipartSession, partNumber :: Int, body :: RequestBody }
    deriving ( Generic )

data UploadPartResponse = UploadPartResponse { etag :: ETag }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m UploadPart where
    type (SpacesResponse UploadPart) = UploadPartResponse

    buildRequest UploadPart { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = session ^? field @"bucket"
               , object         = session ^? field @"object"
               , body           = Just body
               , method         = Just PUT
               , overrideRegion = Nothing
               , headers        = mempty
               , queryString    = Just
                     $ H.toQuery [ ("partNumber" :: Text, tshow partNumber)
                                 , ("uploadId", session ^. field @"uploadID")
                                 ]
               , ..
               }

    consumeResponse raw =
        runMaybeT (UploadPartResponse
                   <$> (readEtag =<< lookupHeader raw "etag"))
        >>= \case
            Nothing -> throwM $ OtherError "Missing/malformed headers"
            Just r  -> return r

-- | Complete a multipart session
data CompleteMultipart = CompleteMultipart
    { session :: MultipartSession
    , parts   :: [(Int, ETag)]
      -- ^ The part numbers and 'ETag's of each uploaded part
    }
    deriving ( Show, Eq, Generic )

data CompleteMultipartResponse = CompleteMultipartResponse
    { location :: Text
    , bucket   :: Bucket
    , object   :: Object
    , etag     :: ETag
      -- ^ The MD5 hash of the final object, i.e. all of the cumulative
      -- uploaded parts
    }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m CompleteMultipart where
    type (SpacesResponse CompleteMultipart) = CompleteMultipartResponse

    buildRequest CompleteMultipart { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = session ^? field @"bucket"
               , object         = session ^? field @"object"
               , method         = Just POST
               , overrideRegion = Nothing
               , headers        = mempty
               , queryString    = Just
                     $ H.toQuery [ ( "uploadId" :: Text
                                   , session ^. field @"uploadID"
                                   )
                                 ]
               , ..
               }
      where
        body               = Just . RequestBodyLBS . X.renderLBS X.def
            $ X.Document prologue root mempty

        prologue           = X.Prologue mempty Nothing mempty

        root               =
            X.Element "CompleteMultipartUpload" mempty (partNode <$> parts)

        partNode (n, etag) = X.NodeElement
            $ X.Element "Part"
                        mempty
                        [ mkNode "PartNumber" (tshow n)
                        , mkNode "ETag" (quote etag)
                        ]

        mkNode name nc =
            X.NodeElement $ X.Element name mempty [ X.NodeContent nc ]

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        bucket <- bucketP cursor
        location <- X.force (xmlElemError "Location")
            $ cursor $/ X.laxElement "Location" &/ X.content
        object <- objectP cursor
        etag <- etagP cursor
        return CompleteMultipartResponse { .. }

-- | Cancel an active multipart upload session
newtype CancelMultipart = CancelMultipart { session :: MultipartSession }
    deriving ( Show, Eq, Generic )

type CancelMultipartResponse = ()

instance MonadSpaces m => Action m CancelMultipart where
    type (SpacesResponse CancelMultipart) = CancelMultipartResponse

    buildRequest CancelMultipart { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = session ^? field @"bucket"
               , object         = session ^? field @"object"
               , method         = Just DELETE
               , body           = Nothing
               , overrideRegion = Nothing
               , headers        = mempty
               , queryString    = Just
                     $ H.toQuery [ ( "uploadId" :: Text
                                   , session ^. field @"uploadID"
                                   )
                                 ]
               , ..
               }

    consumeResponse _ = return ()

-- | List all of the 'Part's of a multipart upload session
newtype ListParts = ListParts { session :: MultipartSession }
    deriving ( Show, Eq, Generic )

data ListPartsResponse = ListPartsResponse
    { bucket         :: Bucket
    , object         :: Object
    , uploadID       :: UploadID
    , parts          :: Seq Part
    , partMarker     :: Int
      -- ^ Part number marking the beginning of the list
    , nextPartMarker :: Int
      -- ^ If truncated, the list location where the next response will begin
    , maxParts       :: Int
    , isTruncated    :: Bool
    }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m ListParts where
    type (SpacesResponse ListParts) = ListPartsResponse

    buildRequest ListParts { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = session ^? field @"bucket"
               , object         = session ^? field @"object"
               , method         = Nothing
               , body           = Nothing
               , overrideRegion = Nothing
               , headers        = mempty
               , queryString    = Just
                     $ H.toQuery [ ( "uploadId" :: Text
                                   , session ^. field @"uploadID"
                                   )
                                 ]
               , ..
               }

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        bucket <- bucketP cursor
        object <- objectP cursor
        uploadID <- X.force (xmlElemError "UploadId")
            $ cursor $/ X.laxElement "UploadId" &/ X.content
        isTruncated <- isTruncP cursor
        maxParts <- xmlNum "MaxParts" cursor
        parts <- S.fromList
            <$> sequence (cursor $/ X.laxElement "Part" &| partP)
        partMarker <- xmlNum "PartNumberMarker" cursor
        nextPartMarker <- xmlNum "NextPartNumberMarker" cursor

        return ListPartsResponse { .. }
      where
        partP c = Part <$> xmlNum "PartNumber" c
            <*> lastModifiedP c
            <*> etagP c
            <*> xmlNum "Size" c