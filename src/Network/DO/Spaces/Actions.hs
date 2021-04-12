{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces.Actions
    ( runAction
      -- * Re-exports
    , module M
    , parseErrorResponse
    ) where

import           Conduit
                 ( (.|)
                 , runConduit
                 )

import           Control.Monad                               ( when )
import           Control.Monad.Catch
                 ( MonadThrow
                 , throwM
                 )
import           Control.Monad.IO.Class                      ( MonadIO(liftIO)
                                                             )
import           Control.Monad.Reader.Class                  ( MonadReader(ask)
                                                             )

import           Data.Conduit.Binary                         ( sinkLbs )
import           Data.Function                               ( (&) )
import           Data.Time                                   ( getCurrentTime
                                                             )

import           Network.DO.Spaces.Actions.CreateBucket      as M
import           Network.DO.Spaces.Actions.DeleteBucket      as M
import           Network.DO.Spaces.Actions.GetBucketLocation as M
import           Network.DO.Spaces.Actions.GetObjectInfo     as M
import           Network.DO.Spaces.Actions.ListAllBuckets    as M
import           Network.DO.Spaces.Actions.ListBucket        as M
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
                 )
import           Network.DO.Spaces.Utils
                 ( handleMaybe
                 , xmlAttrError
                 , xmlDocCursor
                 )
import           Network.HTTP.Client.Conduit                 ( withResponse )
import qualified Network.HTTP.Conduit                        as H
import qualified Network.HTTP.Types                          as H
import           Network.HTTP.Types                          ( Status )

import qualified Text.XML.Cursor                             as X
import           Text.XML.Cursor                             ( ($/), (&/) )

-- | Run an instance of 'Action', receiving a 'SpacesResponse'
runAction
    :: forall a m. (MonadSpaces m, Action a) => a -> m (SpacesResponse a)
runAction action = do
    spaces <- ask
    now <- liftIO getCurrentTime
    req <- newSpacesRequest (buildRequest spaces action) now
    let stringToSign = mkStringToSign req
        auth         = mkAuthorization req stringToSign
        finalized    = finalize req auth
    withResponse @_ @m finalized $ \resp -> do
        let status  = resp & H.responseStatus
            body    = resp & H.responseBody
            headers = resp & H.responseHeaders
            raw     = RawResponse { .. }

        when ((status & H.statusCode) >= 300)
            $ handleMaybe (parseErrorResponse status) raw >>= \case
                Just apiErr -> throwM apiErr
                Nothing     -> throwM . HTTPStatus status
                    =<< runConduit (body .| sinkLbs)

        consumeResponse @a raw

parseErrorResponse
    :: (MonadThrow m, MonadIO m) => Status -> RawResponse m -> m APIException
parseErrorResponse status raw = do
    cursor <- xmlDocCursor raw
    code <- X.force (xmlAttrError "Code")
        $ cursor $/ X.laxElement "Code" &/ X.content
    requestID <- X.force (xmlAttrError "RequestId")
        $ cursor $/ X.laxElement "RequestId" &/ X.content
    hostID <- X.force (xmlAttrError "HostId")
        $ cursor $/ X.laxElement "HostId" &/ X.content
    return APIException { .. }
