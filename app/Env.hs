module Env (Secrets (..), initSecrets) where

import Data.Aeson (Value (Bool))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Relude
import System.Environment (getEnv)

data Secrets = Secrets
  { secretPostmarkKey :: BS.ByteString,
    secretServerUrl :: T.Text,
    secretPassphrase :: T.Text,
    secretServerPort :: Int
  }

initSecrets :: IO Secrets
initSecrets = do
  postmarkAPIKey <- encodeUtf8 <$> getEnv "POSTMARK_API_KEY"
  serverUrl <- T.pack <$> getEnv "NEWSLETTER_SERVER_URL"
  passphrase <- T.pack <$> getEnv "PASSPHRASE"
  -- default to port 3000
  port <- fmap (fromMaybe 3000 . readMaybe) (getEnv "NEWSLETTER_SERVER_PORT")
  return $ Secrets postmarkAPIKey serverUrl passphrase port
