{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Network.DO.Spaces.Actions ( runAction ) where

import           Conduit                     ( MonadUnliftIO )

import           Control.Monad.Catch         ( MonadThrow )
import           Control.Monad.IO.Class      ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class  ( MonadReader(ask) )

import           Data.Time                   ( getCurrentTime )

import           Network.DO.Spaces.Request
                 ( finalize
                 , mkAuthorization
                 , mkStringToSign
                 , newSpacesRequest
                 )
import           Network.DO.Spaces.Types
                 ( Action(..)
                 , MonadSpaces
                 , Spaces
                 )
import           Network.HTTP.Client.Conduit ( withResponse )
import qualified Network.HTTP.Conduit        as H

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
        let status = H.responseStatus resp
            body   = H.responseBody resp
        consumeResponse @a body
