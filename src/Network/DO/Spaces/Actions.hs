{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces.Actions
    ( runAction
      -- * Re-exports
    , module M
    ) where

import           Conduit
                 ( (.|)
                 , runConduit
                 )

import           Control.Monad                               ( when )
import           Control.Monad.Catch                         ( throwM )
import           Control.Monad.IO.Class                      ( MonadIO(liftIO)
                                                             )
import           Control.Monad.Reader.Class                  ( MonadReader(ask)
                                                             )

import qualified Data.ByteString.Lazy                        as LB
import           Data.Conduit.Binary                         ( sinkLbs )
import           Data.Function                               ( (&) )
import           Data.Time                                   ( getCurrentTime
                                                             )

import           Network.DO.Spaces.Actions.CreateBucket      as M
import           Network.DO.Spaces.Actions.DeleteBucket      as M
import           Network.DO.Spaces.Actions.GetBucketLocation as M
import           Network.DO.Spaces.Actions.ListAllBuckets    as M
import           Network.DO.Spaces.Actions.ListBucket        as M
import           Network.DO.Spaces.Request
                 ( finalize
                 , mkAuthorization
                 , mkStringToSign
                 , newSpacesRequest
                 )
import           Network.DO.Spaces.Types
                 ( Action(..)
                 , MonadSpaces
                 , SpacesException(HTTPStatus)
                 )
import           Network.HTTP.Client.Conduit                 ( withResponse )
import qualified Network.HTTP.Conduit                        as H
import qualified Network.HTTP.Types                          as H

-- | Run an 'Action', receiving a 'SpacesResponse'
runAction
    :: forall a m. (MonadSpaces m, Action a) => a -> m (SpacesResponse a)
runAction action = do
    spaces <- ask
    now <- liftIO getCurrentTime
    req <- newSpacesRequest (buildRequest spaces action) now
    let stringToSign = mkStringToSign req
        auth         = mkAuthorization req stringToSign
        finalized    = finalize req auth
    withResponse @_ @IO finalized $ \resp -> do
        let statusCode = resp & H.responseStatus & H.statusCode
            body       = H.responseBody resp
        when (statusCode >= 300) $ do
            b <- LB.toStrict <$> (liftIO . runConduit $ body .| sinkLbs)
            throwM $ HTTPStatus statusCode b
        consumeResponse @a body
