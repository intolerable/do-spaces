{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module Network.DO.Spaces ( newSpaces ) where

import           Control.Exception       ( throwIO )

import qualified Data.ByteString.Char8   as C
import           Data.Foldable           ( asum )
import qualified Data.Text               as T

import           Network.DO.Spaces.Types
                 ( AccessKey(..)
                 , CredentialSource(..)
                 , Region
                 , SecretKey(..)
                 , Spaces(..)
                 , SpacesException(MissingKeys)
                 )
import           Network.HTTP.Client.TLS ( getGlobalManager )

import           System.Environment      ( lookupEnv )

-- | Create a new 'Spaces' with both credentials and HTTP 'Manager'
newSpaces :: Region -> CredentialSource -> IO Spaces
newSpaces region cs = do
    manager <- getGlobalManager
    (accessKey, secretKey) <- source cs
    return Spaces { .. }
  where
    source (Explicit ak sk)              = return (ak, sk)
    source (InEnv (Just (akEnv, skEnv))) =
        ensureKeys =<< (,) <$> lookupKey akEnv <*> lookupKey skEnv
    source (InEnv Nothing)               = do
        ak <- lookupKeys [ "AWS_ACCESS_KEY_ID"
                         , "SPACES_ACCESS_KEY_ID"
                         , "SPACES_ACCESS_KEY"
                         ]
        sk <- lookupKeys [ "AWS_SECRET_ACCESS_KEY"
                         , "SPACES_SECRET_ACCESS_KEY"
                         , "SPACES_SECRET_KEY"
                         ]
        ensureKeys (ak, sk)

    throwMissingKeys k = throwIO . MissingKeys $ "Missing " <> k

    lookupKeys xs = asum <$> (sequence $ lookupEnv <$> xs)

    lookupKey = lookupEnv . T.unpack

    ensureKeys = \case
        (Just a, Just s) -> return (mkKey AccessKey a, mkKey SecretKey s)
        (Just _, _)      -> throwMissingKeys "secret key"
        (_, Just _)      -> throwMissingKeys "access key"
        (_, _)           -> throwMissingKeys "secret and access keys"

    mkKey f = f . C.pack
