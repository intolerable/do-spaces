{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Network.DO.Spaces.Utils
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Small utilities
module Network.DO.Spaces.Utils
    ( -- * General utilities
      tshow
    , bshow
    , unquote
    , quote
    , bodyLBS
    , toLowerBS
    , handleMaybe
    , regionSlug
    , showCannedACL
    , renderUploadHeaders
    , defaultUploadHeaders
    , slugToRegion
    , getResponseMetadata
    , mkNode
    , showPermission
      -- * Parsing/reading
      -- ** XML
    , xmlDocCursor
    , xmlInt
    , xmlElemError
    , xmlUTCTime
    , xmlNum
    , xmlMaybeElem
    , isTruncP
    , bucketP
    , objectP
    , etagP
    , ownerP
    , lastModifiedP
    , aclP
    , writeACLSetter
      -- ** Response headers
    , lookupObjectMetadata
    , lookupHeader
    , readEtag
    , readContentLen
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
import           Data.Either.Extra         ( eitherToMaybe )
import           Data.Generics.Product     ( HasField'(field')
                                           , HasField(field)
                                           )
import qualified Data.Map                  as M
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

import           Lens.Micro                ( (&), (^.) )

import           Network.DO.Spaces.Types
import           Network.HTTP.Conduit
                 ( RequestBody(RequestBodyBS, RequestBodyLBS)
                 )
import           Network.HTTP.Types        ( Header, HeaderName, Status )

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

slugToRegion :: MonadThrow m => Text -> m Region
slugToRegion = \case
    "nyc3" -> return NewYork
    "ams3" -> return Amsterdam
    "sfo3" -> return SanFrancisco
    "sgp1" -> return Singapore
    "fra1" -> return Frankfurt
    reg    -> throwM . OtherError $ "Unrecognized region " <> quote reg

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

showCannedACL :: IsString a => CannedACL -> a
showCannedACL = \case
    Private    -> "private"
    PublicRead -> "public-read"

showPermission :: IsString a => Permission -> a
showPermission = \case
    ReadOnly    -> "READ"
    FullControl -> "FULL_CONTROL"

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

-- | Create an XML 'Node'
mkNode :: X.Name -> Text -> Node
mkNode name nc = X.NodeElement $ X.Element name mempty [ X.NodeContent nc ]

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

-- | Read a 'UTCTime' from an ISO-O8601-formatted 'Text'
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

lookupObjectMetadata :: MonadThrow m => RawResponse m -> m ObjectMetadata
lookupObjectMetadata raw = do
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

parseAmzTime :: [Char] -> Maybe UTCTime
parseAmzTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %EZ"

-- | Lookup the value of a 'HeaderName' from a 'RawResponse' in a monadic context
lookupHeader :: Monad m => RawResponse m -> HeaderName -> MaybeT m ByteString
lookupHeader raw = MaybeT . return . flip lookup (raw ^. field @"headers")

-- | Transform a 'Header' value into an 'ETag'
readEtag :: Monad m => ByteString -> MaybeT m ETag
readEtag = MaybeT . return . fmap unquote . eitherToMaybe . T.decodeUtf8'

-- | Transform a 'Header' value into an 'Int' (for @Content-Length@)
readContentLen :: Monad m => ByteString -> MaybeT m Int
readContentLen = MaybeT . return . readMaybe @Int . C.unpack

aclP :: MonadThrow m => Cursor Node -> m ACLResponse
aclP cursor = ACLResponse
    <$> (X.forceM (xmlElemError "Owner")
         $ cursor $/ X.laxElement "Owner" &| ownerP)
    <*> (X.force (xmlElemError "AccessControlList")
         $ cursor $/ X.laxElement "AccessControlList" &| grantsP)
  where
    grantsP c = X.forceM (xmlElemError "Grant") . sequence
        $ c $/ X.laxElement "Grant" &| grantP

    grantP c = Grant
        <$> (X.forceM (xmlElemError "Permission")
             $ c $/ X.laxElement "Permission" &/ X.content &| readPerm)
        <*> (X.forceM (xmlElemError "Grantee")
             $ c $/ X.laxElement "Grantee" &| granteeP)
      where
        readPerm = \case
            "FULL_CONTROL" -> return FullControl
            "READ"         -> return ReadOnly
            _              ->
                throwM $ InvalidXML "Unrecognized ACL Permission"

    granteeP c = case X.node c of
        X.NodeElement (X.Element _ as _) -> case M.lookup typeName as of
            Just "Group" -> return Group
            Just "CanonicalUser" -> CanonicalUser <$> ownerP c
            _ -> throwM $ InvalidXML "Invalid ACL Grantee type"
        _ -> throwM $ InvalidXML "Invalid ACL Grantee"
      where
        typeName = X.Name "type"
                          (Just "http://www.w3.org/2001/XMLSchema-instance")
                          (Just "xsi")

writeACLSetter :: (HasField' "owner" r Owner, HasField' "acls" r [Grant])
               => r
               -> LB.ByteString
writeACLSetter r = X.renderLBS X.def $ X.Document prologue root mempty
  where
    prologue = X.Prologue mempty Nothing mempty

    root = X.Element policyName mempty nodes
      where
        policyName = X.Name "AccessControlPolicy"
                            (Just "http://s3.amazonaws.com/doc/2006-03-01/")
                            Nothing

    nodes = [ X.NodeElement
              $ X.Element "Owner"
                          mempty
                          [ mkNode "ID"
                                   (r ^. field' @"owner" . field @"id'"
                                    & coerce @_ @Int
                                    & tshow)
                          ]
            , X.NodeElement
              $ X.Element "AccessControlList"
                          mempty
                          (aclNode <$> r ^. field' @"acls")
            ]

    aclNode Grant { .. } = X.NodeElement
        $ X.Element "Grant"
                    mempty
                    [ granteeNode grantee
                    , mkNode "Permission" (showPermission permission)
                    ]

    granteeNode = \case
        CanonicalUser owner -> X.NodeElement
            $ X.Element "Grantee"
                        (granteeAttrs "CanonicalUser")
                        [ mkNode "ID"
                                 (owner ^. field @"id'"
                                  & coerce @_ @Int
                                  & tshow)
                        ]
        Group               -> X.NodeElement
            $ X.Element "Grantee"
                        (granteeAttrs "Group")
                        [ mkNode "URI"
                                 "http://acs.amazonaws.com/groups/global/AllUsers"
                        ]
      where
        granteeAttrs ty =
            M.fromList [ ( X.Name "type"
                                  (Just "http://www.w3.org/2001/XMLSchema-instance")
                                  (Just "xsi")
                         , ty
                         )
                       ]

defaultUploadHeaders :: UploadHeaders
defaultUploadHeaders = UploadHeaders
    { acl                = Nothing
    , cacheControl       = Nothing
    , contentDisposition = Nothing
    , contentEncoding    = Nothing
    , metadata           = mempty
    }

-- | Create a 'SpacesMetadata' by reading response 'Header's, after passing the
-- 'Status'
getResponseMetadata :: Status -> RawResponse m -> SpacesMetadata
getResponseMetadata status RawResponse { .. } = SpacesMetadata { .. }
  where
    requestID =
        eitherToMaybe . T.decodeUtf8' =<< lookup "x-amz-request-id" headers

    date      = parseAmzTime . C.unpack =<< lookup "Date" headers
