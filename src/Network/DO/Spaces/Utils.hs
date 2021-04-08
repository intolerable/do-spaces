{-# LANGUAGE LambdaCase #-}

-- |
module Network.DO.Spaces.Utils
    ( regionSlug
    , toLowerBS
    , xmlIntP
    , xmlAttrError
    , xmlDatetimeP
    ) where

import           Control.Monad.Catch      ( MonadThrow(throwM) )

import           Data.ByteString          ( ByteString )
import qualified Data.ByteString.Char8    as C
import           Data.Char                ( toLower )
import           Data.String              ( IsString )
import           Data.Text                ( Text )
import qualified Data.Text                as T
import           Data.Time                ( UTCTime )
import           Data.Time.Format.ISO8601 ( iso8601ParseM )

import           Network.DO.Spaces.Types

import           Text.Read                ( readMaybe )

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
xmlAttrError :: Text -> SpacesException
xmlAttrError txt = InvalidXML $ "Missing " <> txt
