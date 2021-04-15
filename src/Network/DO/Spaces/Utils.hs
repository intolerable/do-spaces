{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces.Utils
    ( regionSlug
    , toLowerBS
    , xmlIntP
    , xmlAttrError
    , xmlUTCTimeP
    , bshow
    , ownerP
    , xmlDocCursor
    , xmlMaybeAttr
    , showCannedACL
    , handleMaybe
    , unquote
    , eitherToMaybe
    , quote
    , etagP
    , lastModifiedP
    , objectMetadataP
    , bodyLBS
    , tshow
    ) where

import           Conduit                   ( (.|), runConduit )

import           Control.Monad.Catch
                 ( MonadCatch
                 , MonadThrow(throwM)
                 , handleAll
                 )
import           Control.Monad.IO.Class    ( MonadIO )
import           Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )

import           Data.ByteString           ( ByteString )
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as LB
import           Data.Char                 ( toLower )
import           Data.Generics.Product     ( HasField(field) )
import           Data.Maybe                ( listToMaybe )
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

xmlDocCursor :: (MonadIO m, MonadThrow m) => RawResponse m -> m X.Cursor
xmlDocCursor RawResponse { .. } = X.fromDocument
    <$> runConduit (body .| X.sinkDoc X.def)

-- | XML parser for 'Owner' attribute
ownerP :: MonadThrow m => Cursor Node -> m Owner
ownerP c = do
    id' <- X.forceM (xmlAttrError "ID")
        $ c $/ X.laxElement "ID" &/ X.content &| xmlIntP @_ @OwnerID
    return Owner { displayName = id', id' }

-- | XML parser for 'ETag' attribute
etagP :: MonadThrow m => Cursor Node -> m ETag
etagP c = X.force (xmlAttrError "ETag")
    $ c $/ X.laxElement "ETag" &/ X.content &| unquote

-- | XML parser for @LastModified@ attribute
lastModifiedP :: MonadThrow m => Cursor Node -> m UTCTime
lastModifiedP c = X.forceM (xmlAttrError "LastModified")
    $ c $/ X.laxElement "LastModified" &/ X.content &| xmlUTCTimeP

-- | Parse some 'Num' type from 'Text'
xmlIntP :: (MonadThrow m, Num a) => Text -> m a
xmlIntP txt = case readMaybe $ T.unpack txt of
    Just n  -> return $ fromInteger n
    Nothing -> throwM $ InvalidXML "Failed to read integer value"

-- | Parse 'UTCTime' from an 'ISO8601'-formatted 'Text'
xmlUTCTimeP :: MonadThrow m => Text -> m UTCTime
xmlUTCTimeP txt = case iso8601ParseM $ T.unpack txt of
    Just t  -> return t
    Nothing -> throwM $ InvalidXML "Failed to read ISO-8601 value"

-- | Helper to build exceptions during XML parsing
xmlAttrError :: Text -> ClientException
xmlAttrError txt = InvalidXML $ "Missing " <> txt

xmlMaybeAttr :: Cursor Node -> Text -> Maybe Text
xmlMaybeAttr cursor name =
    listToMaybe $ cursor $/ X.laxElement name &/ X.content

objectMetadataP :: MonadThrow m => RawResponse m -> m ObjectMetadata
objectMetadataP raw = do
    metadata <- runMaybeT
        $ ObjectMetadata <$> (readLen =<< lookupHeader "Content-Length")
        <*> lookupHeader "Content-Type"
        <*> (readEtag =<< lookupHeader "Etag")
        <*> (readDate =<< lookupHeader "Last-Modified")
    case metadata of
        Just md -> return md
        Nothing -> throwM $ OtherError "Missing/malformed headers"
  where
    lookupHeader h = MaybeT . return $ lookup h (raw ^. field @"headers")

    readLen        = MaybeT . return . readMaybe @Int . C.unpack

    readDate       = MaybeT . return . parseAmzTime . C.unpack

    readEtag       =
        MaybeT . return . fmap unquote . eitherToMaybe . T.decodeUtf8'

    parseAmzTime   =
        parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %EZ"
