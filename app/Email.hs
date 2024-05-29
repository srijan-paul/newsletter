{-# LANGUAGE NamedFieldPuns #-}

module Email (sendSubscribedEmail) where

import Control.Arrow (ArrowChoice (left))
import Control.Monad.Except (ExceptT, liftEither, liftIO)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Tuple.Extra (both)
import Db.Core (Subscriber (..))
import Env (Secrets (..))
import Network.HTTP.Simple
import Network.HTTP.Types.URI (urlEncode)
import Relude
import System.Directory (doesFileExist)
import System.FilePath (FilePath, (</>))
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Mustache

templatesDir = "templates"

type ErrorMessage = String
type EitherIO a = ExceptT ErrorMessage IO a

-- | Read a file from the file system, and return its contents as a Text.
tryReadFile :: FilePath -> EitherIO T.Text
tryReadFile filePath = do
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then liftIO $ decodeUtf8 <$> readFileLBS filePath
    else liftEither $ Left ("File not found: " <> filePath)

-- | Load a mustache template from its file name relative to the templates directory.
-- @fileName@ The name of the file to load, relative to the templates directory.
loadTemplate :: FilePath -> EitherIO Mustache.Template
loadTemplate fileName = do
  templateText <- tryReadFile (templatesDir </> fileName)
  let templateOrError = Mustache.compileTemplate fileName templateText
  liftEither $ left show templateOrError

-- | Prepare the URL to unsubscribe a user from the newsletter.
-- @serverUrl@: The URL of the server where the unsubscribe endpoint is hosted. (e.g: https://newsletter.injuly.in)
getUnsubUrl :: T.Text -> Subscriber -> T.Text
getUnsubUrl serverUrl (Subscriber {subscriberId, subscriberEmail}) =
  let (subId, subEmail) = both encodeForUrl (subscriberId, subscriberEmail)
   in serverUrl <> "/unsubscribe?id=" <> subId <> "&email=" <> subEmail 
  where
    encodeForUrl :: T.Text -> T.Text
    encodeForUrl = decodeUtf8 . urlEncode True . encodeUtf8

data PostmarkRequest = PostmarkRequest
  { postmarkFrom :: T.Text,
    postmarkTo :: T.Text,
    postmarkSubject :: T.Text,
    postmarkBody :: T.Text,
    postmarkMessageStream :: T.Text
  }

instance ToJSON PostmarkRequest where
  toJSON (PostmarkRequest from to subject htmlBody messageStream) =
    object
      [ "From" .= from,
        "To" .= to,
        "Subject" .= subject,
        "HtmlBody" .= htmlBody,
        "MessageStream" .= messageStream
      ]

-- | Send an email using the postmark HTTPs API.
-- @apikey@ The API key for the postmark account.
-- @to@ The email address of the recipient.
-- @subject@ The subject of the email.
-- @body@ The body of the email.
-- @stream@ The message stream to send the email to.
sendEmail :: ByteString -> T.Text -> T.Text -> T.Text -> T.Text -> EitherIO ()
sendEmail apikey to subject body stream =
  do
    -- Postmark free tier has a limit of 100 emails per month.
    -- Once I have more users than that, I will need to either pay 15$ a month,
    -- or roll my own mailserver.
    req' <- parseRequest "POST https://api.postmarkapp.com/email"
    let headers =
          [ ("Content-Type", "application/json"),
            ("X-Postmark-Server-Token", apikey),
            ("Accept", "application/json")
          ]
        postMarkReq = PostmarkRequest "srijan@injuly.in" to subject body stream
        request = setRequestHeaders headers (setRequestBodyJSON postMarkReq req')
    response <- httpNoBody request
    return ()

-- | Send a welcome email to a new subscriber.
-- @secrets@ The secrets required to send the email.
-- @subscriber@ The subscriber to send the email to.
sendSubscribedEmail :: Secrets -> Subscriber -> EitherIO ()
sendSubscribedEmail (Secrets {secretPostmarkKey, secretServerUrl}) subscriber = do
  template <- loadTemplate "subscribed.mustache"
  let (email, unsubUrl) = (subscriberEmail subscriber, getUnsubUrl secretServerUrl subscriber)
      compileData = Mustache.object [("unsubscribe_url", Mustache.String unsubUrl)]
      htmlText = Mustache.substitute template compileData
  sendEmail secretPostmarkKey email "This is a test!" htmlText "on-subscribe"


