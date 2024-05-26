import Control.Arrow (ArrowChoice (left))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (Format (formatShowM), ISO8601 (iso8601Format))
import Db.Core (Db, OpenDb, Subscriber (..), insertSubscriber, openDB, unsubscribeUser)
import Email (sendSubscribedEmail)
import Network.HTTP.Types.Status (mkStatus)
import qualified Network.Mail.Mime as Mail
import Relude
import qualified Text.Email.Validate as Validate
import Web.Scotty

newtype App = App {db :: OpenDb}

main :: IO ()
main = do
  db <- openDB
  scotty 3000
    $ do
      let app = App db
      post "/subscribe" (subscribeEndpoint app)
      post "/unsubscribe" (unsubscribeEndpoint app)

badRequest :: Text -> ActionM ()
badRequest messageText = do
  let message = encodeUtf8 messageText
  status $ mkStatus 400 message
  text $ TL.fromStrict messageText
  finish

sendFirstMail :: Subscriber -> ActionM ()
sendFirstMail subscriber = do
  result <- liftIO (runExceptT $ sendSubscribedEmail subscriber)
  case result of
    -- TODO: notify myself in a log fiel that the email failed.
    Left err -> putStrLn $ "Error sending email: " <> err
    Right _ -> return ()

subscribeEndpoint :: App -> ActionM ()
subscribeEndpoint (App db) = do
  emailAddrByteStr <- BS.toStrict <$> body
  if Validate.isValid emailAddrByteStr
    then do
      now <- liftIO getCurrentTime
      let timeS = fromMaybe (show now) (formatShowM iso8601Format now)
          emailAddr = decodeUtf8 emailAddrByteStr :: T.Text
          newSub =
            Subscriber
              { subscriberIsSubbed = True,
                subscriberId = "dummyId",
                subscriberEmail = emailAddr,
                subscriberDate = T.pack timeS
              }

      liftIO $ insertSubscriber db newSub
      sendFirstMail newSub
      putStrLn $ "New Subscriber: " <> T.unpack emailAddr
    else
      badRequest "Invalid email address"

unsubscribeEndpoint :: App -> ActionM ()
unsubscribeEndpoint (App db) = do
  subscriberId <- queryParamMaybe "id" :: ActionM (Maybe T.Text)
  case subscriberId of
    Nothing -> badRequest "Missing id parameter"
    Just id -> liftIO (unsubscribeUser db id)

sendMailEndpoint :: ActionM ()
sendMailEndpoint = do
  requestBody <- (decodeUtf8 <$> body) :: ActionM T.Text
  error "not implemented"
