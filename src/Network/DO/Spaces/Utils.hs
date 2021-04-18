{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces.Utils
    ( regionSlug
    , toLowerBS
    , xmlInt
    , xmlElemError
    , xmlUTCTime
    , bshow
    , ownerP
    , xmlDocCursor
    , xmlMaybeElem
    , showCannedACL
    , handleMaybe
    , unquote
    , eitherToMaybe
    , quote
    , etagP
    , lastModifiedP
    , getObjectMetadata
    , bodyLBS
    , tshow
    , lookupHeader
    , readEtag
    , readContentLen
    , renderUploadHeaders
    , isTruncP
    , xmlNum
    , bucketP
    , objectP
    ) where

import           Conduit                   ( (.|), runConduit )

import           Control.Monad.Catch
                 ( MonadCatch
                 , MonadThrow(throwM)
                 , handleAll
                 )
import           Control.Monad.IO.Class    ( MonadIO )
import           Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )

import           Data.Bifunctor            ( Bifunctor(first, second) )
import           Data.Bool                 ( bool )
import           Data.ByteString           ( ByteString )
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as LB
import qualified Data.CaseInsensitive      as CI
import           Data.Char                 ( toLower )
import           Data.Coerce               ( coerce )
import           Data.Generics.Product     ( HasField(field) )
import           Data.Maybe                ( catMaybes, listToMaybe )
import           Data.String               ( IsString )
import           Data.Text                 ( Text )
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Time
                 ( UTCTime
                 , defaultTimeLocale
                 , parseTimeM
                 )
import           Data.Time.Format.ISO8601  ( iso8601ParseM )

import           Lens.Micro                ( (^.) )

import           Network.DO.Spaces.Types
import           Network.HTTP.Conduit
                 ( RequestBody(RequestBodyBS, RequestBodyLBS)
                 )
import           Network.HTTP.Types        ( Header, HeaderName )

import           Text.Read                 ( readMaybe )
import           Text.XML                  ( Node )
import qualified Text.XML                  as X
import qualified Text.XML.Cursor           as X
import           Text.XML.Cursor           ( ($/), (&/), (&|) )
import           Text.XML.Cursor.Generic   ( Cursor )

-- | Convert a 'Region' to its equivalent slug
regionSlug :: IsString a => Region -> a
regionSlug = \case
    NewYork      -> "nyc3"
    Amsterdam    -> "ams3"
    SanFrancisco -> "sfo3"
    Singapore    -> "sgp1"
    Frankfurt    -> "fra1"

-- | Map 'ByteString' chars to lower-case
toLowerBS :: ByteString -> ByteString
toLowerBS = C.pack . fmap toLower . C.unpack

-- | Show a 'ByteString'
bshow :: Show a => a -> ByteString
bshow = C.pack . show

-- | Show some 'Text'
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Strip leading and trailing double quotes from a 'Text'
unquote :: Text -> Text
unquote = T.dropAround ('"' ==)

quote :: (IsString a, Monoid a) => a -> a
quote x = "\"" <> x <> "\""

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _         = Nothing

showCannedACL :: IsString a => CannedACL -> a
showCannedACL = \case
    Private    -> "private"
    PublicRead -> "public-read"

handleMaybe :: MonadCatch m => (a -> m b) -> a -> m (Maybe b)
handleMaybe g x = handleAll (\_ -> return Nothing) (Just <$> g x)

-- | Convert a 'RequestBody' to a 'Data.ByteString.Lazy.ByteString'
bodyLBS :: MonadThrow m => RequestBody -> m LB.ByteString
bodyLBS (RequestBodyBS b)   = return $ LB.fromStrict b
bodyLBS (RequestBodyLBS lb) = return lb
bodyLBS _                   =
    throwM $ InvalidRequest "Unsupported request body type"

-- | Convert 'UploadHeaders' to a list of request 'Header's
renderUploadHeaders :: UploadHeaders -> [Header]
renderUploadHeaders UploadHeaders { .. } = second T.encodeUtf8
    <$> catMaybes [ ("x-amz-acl", ) . showCannedACL <$> acl
                  , ("Cache-Control", ) <$> cacheControl
                  , ("Content-Disposition", ) <$> contentDisposition
                  , ("Content-Encoding", ) <$> contentEncoding
                  ]
    <> (first (CI.mk . T.encodeUtf8 . ("x-amz-meta-" <>)) <$> metadata)

xmlDocCursor :: (MonadIO m, MonadThrow m) => RawResponse m -> m X.Cursor
xmlDocCursor RawResponse { .. } = X.fromDocument
    <$> runConduit (body .| X.sinkDoc X.def)

-- | XML parser for 'Owner' attribute
ownerP :: MonadThrow m => Cursor Node -> m Owner
ownerP c = do
    id' <- X.forceM (xmlElemError "ID")
        $ c $/ X.laxElement "ID" &/ X.content &| xmlInt @_ @OwnerID
    return Owner { displayName = id', id' }

-- | XML parser for 'ETag' attribute
etagP :: MonadThrow m => Cursor Node -> m ETag
etagP c = X.force (xmlElemError "ETag")
    $ c $/ X.laxElement "ETag" &/ X.content &| unquote

-- | XML parser for @LastModified@ attribute
lastModifiedP :: MonadThrow m => Cursor Node -> m UTCTime
lastModifiedP c = X.forceM (xmlElemError "LastModified")
    $ c $/ X.laxElement "LastModified" &/ X.content &| xmlUTCTime

-- | Read a 'Num' type from 'Text'
xmlInt :: (MonadThrow m, Num a) => Text -> m a
xmlInt txt = case readMaybe $ T.unpack txt of
    Just n  -> return $ fromInteger n
    Nothing -> throwM $ InvalidXML "Failed to read integer value"

-- | Read a 'Num' type, encoded as an integer, from XML
xmlNum :: Num a => MonadThrow m => Text -> Cursor Node -> m a
xmlNum name c = X.forceM (xmlElemError name)
    $ c $/ X.laxElement name &/ X.content &| xmlInt

-- | Read a 'UTCTime' from an 'ISO8601'-formatted 'Text'
xmlUTCTime :: MonadThrow m => Text -> m UTCTime
xmlUTCTime txt = case iso8601ParseM $ T.unpack txt of
    Just t  -> return t
    Nothing -> throwM $ InvalidXML "Failed to read ISO-8601 value"

isTruncP :: MonadThrow m => Cursor Node -> m Bool
isTruncP c = X.force (xmlElemError "IsTruncated")
    $ c $/ X.laxElement "IsTruncated" &/ X.content &| truncP
  where
    truncP t = bool False True (t == "true")

-- | Helper to build exceptions during XML parsing
xmlElemError :: Text -> ClientException
xmlElemError txt = InvalidXML $ "Missing " <> txt

-- | Parse the name of a 'Bucket' from XML
bucketP :: MonadThrow m => Cursor Node -> m Bucket
bucketP c = X.force (xmlElemError "Bucket")
    $ c $/ X.laxElement "Bucket" &/ X.content &| coerce

-- | Parse the name of an 'Object' from XML
objectP :: MonadThrow m => Cursor Node -> m Object
objectP c = X.force (xmlElemError "Key")
    $ c $/ X.laxElement "Key" &/ X.content &| coerce

xmlMaybeElem :: Cursor Node -> Text -> Maybe Text
xmlMaybeElem cursor name =
    listToMaybe $ cursor $/ X.laxElement name &/ X.content

getObjectMetadata :: MonadThrow m => RawResponse m -> m ObjectMetadata
getObjectMetadata raw = do
    metadata <- runMaybeT
        $ ObjectMetadata
        <$> (readContentLen =<< lookupHeader' "Content-Length")
        <*> lookupHeader' "Content-Type"
        <*> (readEtag =<< lookupHeader' "Etag")
        <*> (readDate =<< lookupHeader' "Last-Modified")
    case metadata of
        Just md -> return md
        Nothing -> throwM $ OtherError "Missing/malformed headers"
  where
    lookupHeader' = lookupHeader raw

    readDate      = MaybeT . return . parseAmzTime . C.unpack

    parseAmzTime  =
        parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %EZ"

-- | Lookup the value of a 'HeaderName' from a 'RawResponse' in a monadic context
lookupHeader :: Monad m => RawResponse m -> HeaderName -> MaybeT m ByteString
lookupHeader raw = MaybeT . return . flip lookup (raw ^. field @"headers")

-- | Transform a 'Header' value into an 'ETag'
readEtag :: Monad m => ByteString -> MaybeT m ETag
readEtag = MaybeT . return . fmap unquote . eitherToMaybe . T.decodeUtf8'

-- | Transform a 'Header' value into an 'Int' (for @Content-Length@)
readContentLen :: Monad m => ByteString -> MaybeT m Int
readContentLen = MaybeT . return . readMaybe @Int . C.unpack
