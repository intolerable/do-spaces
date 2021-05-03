{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Network.DO.Spaces.Actions
-- Copyright   : (c) 2021 Rory Tyler Hayford
-- License     : BSD-3-Clause
-- Maintainer  : rory.hayford@protonmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module re-exports the 'Action' instances in the modules below it
--
module Network.DO.Spaces.Actions
    ( runAction
    , parseErrorResponse
      -- * Re-exports
    , module M
    ) where

import           Conduit
                 ( (.|)
                 , runConduit
                 )

import           Control.Monad                                ( when )
import           Control.Monad.Catch
                 ( MonadThrow
                 , throwM
                 )
import           Control.Monad.IO.Class                       ( MonadIO(liftIO)
                                                              )

import           Data.Conduit.Binary                          ( sinkLbs )
import           Data.Function                                ( (&) )
import           Data.Time                                    ( getCurrentTime
                                                              )

import           Network.DO.Spaces.Actions.CopyObject         as M
import           Network.DO.Spaces.Actions.CreateBucket       as M
import           Network.DO.Spaces.Actions.DeleteBucket       as M
import           Network.DO.Spaces.Actions.DeleteBucketCORS   as M
import           Network.DO.Spaces.Actions.DeleteObject       as M
import           Network.DO.Spaces.Actions.GetBucketACLs      as M
import           Network.DO.Spaces.Actions.GetBucketCORS      as M
import           Network.DO.Spaces.Actions.GetBucketLifecycle as M
import           Network.DO.Spaces.Actions.GetBucketLocation  as M
import           Network.DO.Spaces.Actions.GetObject          as M
import           Network.DO.Spaces.Actions.GetObjectACLs      as M
import           Network.DO.Spaces.Actions.GetObjectInfo      as M
import           Network.DO.Spaces.Actions.ListAllBuckets     as M
import           Network.DO.Spaces.Actions.ListBucket         as M
import           Network.DO.Spaces.Actions.SetBucketACLs      as M
import           Network.DO.Spaces.Actions.SetBucketCORS      as M
import           Network.DO.Spaces.Actions.SetBucketLifecycle as M
import           Network.DO.Spaces.Actions.SetObjectACLs      as M
import           Network.DO.Spaces.Actions.UploadMultipart    as M
import           Network.DO.Spaces.Actions.UploadObject       as M
import           Network.DO.Spaces.Request
                 ( finalize
                 , mkAuthorization
                 , mkStringToSign
                 , newSpacesRequest
                 )
import           Network.DO.Spaces.Types
                 ( APIException(..)
                 , Action(..)
                 , ClientException(HTTPStatus)
                 , MonadSpaces
                 , RawResponse(..)
                 , SpacesResponse(..)
                 , WithMetadata(NoMetadata, KeepMetadata)
                 )
import           Network.DO.Spaces.Utils
                 ( getResponseMetadata
                 , handleMaybe
                 , xmlDocCursor
                 , xmlElemError
                 )
import           Network.HTTP.Client.Conduit                  ( withResponse )
import qualified Network.HTTP.Conduit                         as H
import qualified Network.HTTP.Types                           as H
import           Network.HTTP.Types                           ( Status )

import qualified Text.XML.Cursor                              as X
import           Text.XML.Cursor                              ( ($/), (&/) )

-- | Run an instance of 'Action', receiving a 'ConsumedResponse'. The retention
-- of 'Network.DO.Spaces.Types.SpacesMetadata' can be controlled by passing a
-- 'Network.DO.Spaces.Types.WithMetadata' constructor
runAction :: forall a m.
          (MonadSpaces m, Action m a)
          => WithMetadata
          -> a
          -> m (SpacesResponse a)
runAction withMD action = do
    now <- liftIO getCurrentTime
    reqBuilder <- buildRequest action
    req <- newSpacesRequest reqBuilder now
    let stringToSign = mkStringToSign req
        auth         = mkAuthorization req stringToSign
        finalized    = finalize req auth

    withResponse @_ @m finalized $ \resp -> do
        let status   = resp & H.responseStatus
            body     = resp & H.responseBody
            headers  = resp & H.responseHeaders
            metadata = case withMD of
                NoMetadata   -> Nothing
                KeepMetadata -> Just $ getResponseMetadata status raw
            raw      = RawResponse { .. }

        when ((status & H.statusCode) >= 300)
            $ handleMaybe (parseErrorResponse status) raw >>= \case
                Just apiErr -> throwM apiErr
                Nothing     -> throwM . HTTPStatus status
                    =<< runConduit (body .| sinkLbs)

        result <- consumeResponse @_ @a raw
        return SpacesResponse { .. }

parseErrorResponse
    :: (MonadThrow m, MonadIO m) => Status -> RawResponse m -> m APIException
parseErrorResponse status raw = do
    cursor <- xmlDocCursor raw
    code <- X.force (xmlElemError "Code")
        $ cursor $/ X.laxElement "Code" &/ X.content
    requestID <- X.force (xmlElemError "RequestId")
        $ cursor $/ X.laxElement "RequestId" &/ X.content
    hostID <- X.force (xmlElemError "HostId")
        $ cursor $/ X.laxElement "HostId" &/ X.content
    return APIException { .. }

