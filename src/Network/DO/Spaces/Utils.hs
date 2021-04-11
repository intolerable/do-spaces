{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces.Utils
    ( regionSlug
    , toLowerBS
    , xmlIntP
    , xmlAttrError
    , xmlDatetimeP
    , bshow
    , ownerP
    , xmlDocCursor
    , xmlMaybeField
    , showCannedACL
    , handleMaybe
    ) where

import           Conduit                  ( (.|), runConduit )

import           Control.Monad.Catch
                 ( MonadCatch
                 , MonadThrow(throwM)
                 , handle
                 , handleAll
                 )
import           Control.Monad.IO.Class   ( MonadIO(liftIO) )

import           Data.ByteString          ( ByteString )
import qualified Data.ByteString.Char8    as C
import           Data.Char                ( toLower )
import           Data.Maybe               ( listToMaybe )
import           Data.String              ( IsString )
import           Data.Text                ( Text )
import qualified Data.Text                as T
import           Data.Time                ( UTCTime )
import           Data.Time.Format.ISO8601 ( iso8601ParseM )

import           Network.DO.Spaces.Types

import           Text.Read                ( readMaybe )
import           Text.XML                 ( Node )
import qualified Text.XML                 as X
import qualified Text.XML.Cursor          as X
import           Text.XML.Cursor          ( ($/), (&/), (&|) )
import           Text.XML.Cursor.Generic  ( Cursor )

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

showCannedACL :: IsString a => CannedACL -> a
showCannedACL = \case
    Private    -> "private"
    PublicRead -> "public-read"

handleMaybe :: MonadCatch m => (a -> m b) -> a -> m (Maybe b)
handleMaybe g x = handleAll (\_ -> return Nothing) (Just <$> g x)

xmlDocCursor :: MonadIO m => RawBody -> m X.Cursor
xmlDocCursor raw = X.fromDocument
    <$> (liftIO . runConduit $ raw .| X.sinkDoc X.def)

-- | XML parser for recurring 'Owner' attribute
ownerP :: MonadThrow m => Cursor Node -> m Owner
ownerP c = do
    id' <- X.forceM (xmlAttrError "Owner ID")
        $ c $/ X.laxElement "ID" &/ X.content &| xmlIntP @_ @ID
    return Owner { displayName = id', id' }

-- | Parse some 'Num' type from 'Text'
xmlIntP :: (MonadThrow m, Num a) => Text -> m a
xmlIntP txt = case readMaybe $ T.unpack txt of
    Just n  -> return $ fromInteger n
    Nothing -> throwM $ InvalidXML "Failed to read integer value"

-- | Parse 'UTCTime' from an 'ISO8601'-formatted 'Text'
xmlDatetimeP :: MonadThrow m => Text -> m UTCTime
xmlDatetimeP txt = case iso8601ParseM $ T.unpack txt of
    Just t  -> return t
    Nothing -> throwM $ InvalidXML "Failed to read ISO-8601 value"

-- | Helper to build exceptions during XML parsing
xmlAttrError :: Text -> ClientException
xmlAttrError txt = InvalidXML $ "Missing " <> txt

xmlMaybeField :: Cursor Node -> Text -> Maybe Text
xmlMaybeField cursor name =
    listToMaybe $ cursor $/ X.laxElement name &/ X.content
