module Env (Secrets (..), initSecrets) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Relude
import System.Environment (getEnv)

data Secrets = Secrets
  { secretPostmarkKey :: BS.ByteString,
    secretServerUrl :: T.Text
  }

initSecrets :: IO Secrets
initSecrets = do
  postmarkAPIKey <- encodeUtf8 <$> getEnv "POSTMARK_API_KEY"
  serverUrl <- T.pack <$> getEnv "NEWSLETTER_SERVER_URL"
  return $ Secrets postmarkAPIKey serverUrl
