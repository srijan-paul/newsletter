{-# LANGUAGE NamedFieldPuns #-}

import Crypto.Random (MonadRandom (getRandomBytes))
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (Format (formatShowM), ISO8601 (iso8601Format))
import Db.Core (Db, OpenDb, Subscriber (..), insertSubscriber, openDB, unsubscribeUser)
import Email (sendSubscribedEmail)
import Env (Secrets (..), initSecrets)
import Network.HTTP.Types.Status (mkStatus)
import qualified Network.Mail.Mime as Mail
import Relude
import qualified Text.Email.Validate as Validate
import Web.Scotty

data App = App
  { appDatabase :: OpenDb,
    appSecrets :: Secrets
  }

main :: IO ()
main = do
  db <- openDB
  secrets <- initSecrets
  let app = App db secrets
  scotty 3000 $ do
    post "/subscribe" (subscribeEndpoint app)
    post "/unsubscribe" (unsubscribeEndpoint app)

badRequest :: Text -> ActionM ()
badRequest messageText = do
  let message = encodeUtf8 messageText
  status $ mkStatus 400 message
  text $ TL.fromStrict messageText
  finish

sendFirstMail :: Secrets -> Subscriber -> IO ()
sendFirstMail secrets subscriber = do
  result <- runExceptT $ sendSubscribedEmail secrets subscriber
  case result of
    -- TODO: notify myself in a log that the email failed.
    Left err -> putStrLn $ "Error sending email: " <> err
    Right _ -> return ()

-- | Randomly generate an ID for the user.
-- This is ID is sent with the unsubscribe link to make sure that a user can only unsubscribe themselves.
generateRandomUserId :: IO T.Text
generateRandomUserId = do
  randomBytes <- getRandomBytes 34 :: IO ByteString
  let randomString = convertToBase Base64 randomBytes :: ByteString
  return $ decodeUtf8 randomString

subscribeEndpoint :: App -> ActionM ()
subscribeEndpoint App {appDatabase = db, appSecrets} = do
  emailAddrByteStr <- BS.toStrict <$> body
  if Validate.isValid emailAddrByteStr
    then liftIO $ storeSubscriber (decodeUtf8 emailAddrByteStr)
    else badRequest "Invalid email address"
  where
    storeSubscriber :: T.Text -> IO ()
    storeSubscriber emailAddr = do
      now      <- getCurrentTime
      randomId <- generateRandomUserId
      let timeS = fromMaybe (show now) (formatShowM iso8601Format now)
          newSub =
            Subscriber
              { subscriberIsSubbed = True,
                subscriberId = randomId,
                subscriberEmail = emailAddr,
                subscriberDate = T.pack timeS
              }

      insertSubscriber db newSub
      sendFirstMail appSecrets newSub
      putStrLn $ "New Subscriber: " <> T.unpack emailAddr

unsubscribeEndpoint :: App -> ActionM ()
unsubscribeEndpoint App {appDatabase = db} = do
  userId <- queryParamMaybe "id" :: ActionM (Maybe T.Text)
  userEmail <- queryParamMaybe "email" :: ActionM (Maybe T.Text)
  case (userId, userEmail) of
    (Just id, Just email) -> liftIO (unsubscribeUser db id email)
    (Nothing, Nothing) -> badRequest "Missing parameters: id and email"
    (Nothing, _) -> badRequest "Missing parameter: id"
    (_, Nothing) -> badRequest "Missing parameter: email"

sendMailEndpoint :: ActionM ()
sendMailEndpoint = do
  requestBody <- (decodeUtf8 <$> body) :: ActionM T.Text
  error "not implemented"
