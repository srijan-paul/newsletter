{-# LANGUAGE NamedFieldPuns #-}

import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (Format (formatShowM), ISO8601 (iso8601Format))
import Db.Core (Db, OpenDb, Subscriber (..), fetchActiveSubscribers, insertSubscriber, openDB, unsubscribeUser)
import Email (sendSubscribedEmail, sendToAll)
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
      port = secretServerPort secrets
  scotty port $ do
    post "/subscribe" (subscribeEndpoint app)
    post "/unsubscribe" (unsubscribeEndpoint app)

unauthorized :: ActionM ()
unauthorized = do
  status $ mkStatus 401 "Unauthorized"
  finish

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

-- | The endpoint to subscribe a user to the newsletter.
subscribeEndpoint :: App -> ActionM ()
subscribeEndpoint App {appDatabase = db, appSecrets} = do
  emailAddrByteStr <- BS.toStrict <$> body
  if Validate.isValid emailAddrByteStr
    then liftIO $ storeSubscriber (decodeUtf8 emailAddrByteStr)
    else badRequest "Invalid email address"
  where
    storeSubscriber :: T.Text -> IO ()
    storeSubscriber emailAddr = do
      now <- getCurrentTime
      randomId <- generateRandomUserId
      let timeStr = T.pack $ fromMaybe (show now) (formatShowM iso8601Format now)
          newSub =
            Subscriber
              { subscriberIsSubbed = True,
                subscriberId = randomId,
                subscriberEmail = emailAddr,
                subscriberDate = timeStr
              }

      insertSubscriber db newSub
      sendFirstMail appSecrets newSub
      putStrLn $ "New Subscriber: " <> T.unpack emailAddr

-- | The endpoint to unsubscribe a user from the newsletter.
unsubscribeEndpoint :: App -> ActionM ()
unsubscribeEndpoint App {appDatabase = db} = do
  userId <- queryParamMaybe "id" :: ActionM (Maybe T.Text)
  userEmail <- queryParamMaybe "email" :: ActionM (Maybe T.Text)
  case (userId, userEmail) of
    (Just id, Just email) -> liftIO $ unsubscribeUser db id email
    (Nothing, Nothing) -> badRequest "Missing parameters: id and email"
    (Nothing, _) -> badRequest "Missing parameter: id"
    (_, Nothing) -> badRequest "Missing parameter: email"

-- | The body of a POST request made to send an email
data EmailRequest = EmailRequest
  { -- | Subject line of the email.
    emailReqSubject :: T.Text,
    -- | The body of the email in markdown format.
    emailReqBody :: T.Text
  }

instance Json.FromJSON EmailRequest where
  parseJSON = Json.withObject "EmailRequest" $ \o -> do
    subject <- o .: "subject"
    body <- o .: "body"
    return EmailRequest {emailReqSubject = subject, emailReqBody = body}

-- | Send an email to all active subscribers.
sendMailEndpoint :: App -> ActionM ()
sendMailEndpoint App {appSecrets, appDatabase} = do
  requestBody <- body
  authToken <- header "Authorization"

  let passphrase = secretPassphrase appSecrets
      isAuthorized = fmap TL.toStrict authToken == Just passphrase

  when isAuthorized $ do
    case Json.decode @EmailRequest requestBody of
      Just (EmailRequest subject body) -> do
        activeSubs <- liftIO $ fetchActiveSubscribers appDatabase
        result <- liftIO $ runExceptT (sendToAll subject body appSecrets activeSubs)
        case result of
          Left errorMessage -> do
            putStrLn errorMessage
            badRequest $ "Error sending email: " <> T.pack errorMessage
      Nothing -> badRequest "Expected a JSON request body with fields 'subject' and 'body'"
