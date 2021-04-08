{-# LANGUAGE RecordWildCards #-}

-- |
module Network.DO.Spaces ( newSpaces ) where

import           Control.Exception         ( throwIO )
import           Control.Monad.Trans.Maybe ( MaybeT(runMaybeT, MaybeT) )

import qualified Data.ByteString.Char8     as C
import           Data.Foldable             ( asum )
import qualified Data.Text                 as T

import           Network.DO.Spaces.Types
                 ( AccessKey(..)
                 , CredentialSource(..)
                 , Region
                 , SecretKey(..)
                 , Spaces(..)
                 , SpacesException(MissingKeys)
                 )
import           Network.HTTP.Client.TLS   ( getGlobalManager )

import           System.Environment        ( lookupEnv )

-- | Create a new 'Spaces' with both credentials and HTTP 'Manager'
newSpaces :: Region -> CredentialSource -> IO Spaces
newSpaces region cs = do
    manager <- getGlobalManager
    (accessKey, secretKey) <- source cs
    return Spaces { .. }
  where
    source (Explicit ak sk) = return (ak, sk)
    source (InEnv ksM) = case ksM of
        Just (ak, sk) -> do
            a <- lookupEnv $ T.unpack ak
            s <- lookupEnv $ T.unpack sk
            case (a, s) of
                (Just accessKey, Just secretKey) ->
                    return ( AccessKey $ C.pack accessKey
                           , SecretKey $ C.pack secretKey
                           )
                _ -> throwMissingKeys
        Nothing       -> do
            keys <- runMaybeT $ do
                ak <- lookupKeysEnv [ "AWS_ACCESS_KEY_ID"
                                    , "SPACES_ACCESS_KEY_ID"
                                    , "SPACES_ACCESS_KEY"
                                    ]
                sk <- lookupKeysEnv [ "AWS_SECRET_ACCESS_KEY"
                                    , "SPACES_SECRET_ACCESS_KEY"
                                    , "SPACES_SECRET_KEY"
                                    ]
                return (ak, sk)
            case keys of
                Just (ak, sk) ->
                    return (AccessKey $ C.pack ak, SecretKey $ C.pack sk)
                Nothing       -> throwMissingKeys
      where
        lookupKeysEnv    = MaybeT . asum . fmap lookupEnv

        throwMissingKeys = throwIO $ MissingKeys "Missing secret/access key"
