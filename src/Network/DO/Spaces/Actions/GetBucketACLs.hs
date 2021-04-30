{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Network.DO.Spaces.Actions.GetBucketACLs
    ( GetBucketACLs(..)
    , GetBucketACLsResponse(..)
    ) where

import           Control.Monad.Catch     ( MonadThrow(throwM) )
import           Control.Monad.Reader    ( MonadReader(ask) )

import           Data.ByteString         ( ByteString )
import qualified Data.Map                as M

import           GHC.Generics            ( Generic )

import           Network.DO.Spaces.Types
                 ( Action(..)
                 , Bucket
                 , ClientException(InvalidXML)
                 , Grant(..)
                 , Grantee(Group, CanonicalUser)
                 , MonadSpaces
                 , Owner
                 , Permission(..)
                 , SpacesRequestBuilder(..)
                 )
import           Network.DO.Spaces.Utils ( ownerP
                                         , xmlDocCursor
                                         , xmlElemError
                                         )
import qualified Network.HTTP.Types      as H

import qualified Text.XML                as X
import qualified Text.XML.Cursor         as X
import           Text.XML.Cursor         ( ($/), (&/), (&|) )

-- | Get the full Access Control List associated with a 'Bucket'
data GetBucketACLs = GetBucketACLs { bucket :: Bucket }
    deriving ( Show, Eq, Generic )

data GetBucketACLsResponse =
    GetBucketACLsResponse { owner :: Owner, accessControlList :: [Grant] }
    deriving ( Show, Eq, Generic )

instance MonadSpaces m => Action m GetBucketACLs where
    type (ConsumedResponse GetBucketACLs) = GetBucketACLsResponse

    buildRequest GetBucketACLs { .. } = do
        spaces <- ask
        return SpacesRequestBuilder
               { bucket         = Just bucket
               , method         = Nothing
               , body           = Nothing
               , object         = Nothing
               , overrideRegion = Nothing
               , queryString    = Nothing
               , headers        = mempty
               , subresources   = Just
                     $ H.toQuery [ ( "acl" :: ByteString
                                   , Nothing :: Maybe ByteString
                                   )
                                 ]
               , ..
               }

    consumeResponse raw = do
        cursor <- xmlDocCursor raw
        owner <- X.forceM (xmlElemError "Owner")
            $ cursor $/ X.laxElement "Owner" &| ownerP
        accessControlList <- X.force (xmlElemError "AccessControlList")
            $ cursor $/ X.laxElement "AccessControlList" &| grantsP
        return GetBucketACLsResponse { .. }
      where
        grantsP c = X.forceM (xmlElemError "Grant") . sequence
            $ c $/ X.laxElement "Grant" &| grantP

        grantP c = do
            permission <- X.forceM (xmlElemError "Permission")
                $ c $/ X.laxElement "Permission" &/ X.content &| readPerm
            grantee <- X.forceM (xmlElemError "Grantee")
                $ c $/ X.laxElement "Grantee" &| granteeP
            return Grant { .. }
          where
            readPerm = \case
                "FULL_CONTROL" -> return FullControl
                "READ"         -> return ReadOnly
                _              -> throwM
                    $ InvalidXML "GetBucketACLs: unrecognized Permission"

        granteeP c = case X.node c of
            X.NodeElement (X.Element _ as _) -> case M.lookup typeName as of
                Just "Group" -> return Group
                Just "CanonicalUser" -> CanonicalUser <$> ownerP c
                _ -> throwM $ InvalidXML "GetBucketACLs: invalid Grantee type"
            _ -> throwM $ InvalidXML "GetBucketACLs: invalid Grantee"
          where
            typeName =
                X.Name "type"
                       (Just "http://www.w3.org/2001/XMLSchema-instance")
                       (Just "xsi")
