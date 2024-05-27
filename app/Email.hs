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

tryReadFile :: FilePath -> EitherIO T.Text
tryReadFile filePath = do
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then liftIO $ decodeUtf8 <$> readFileLBS filePath
    else liftEither $ Left ("File not found: " <> filePath)

-- | Load a mustache template from its file name relative to the templates directory.
loadTemplate :: FilePath -> EitherIO Mustache.Template
loadTemplate fileName = do
  templateText <- tryReadFile (templatesDir </> fileName)
  let templateOrError = Mustache.compileTemplate fileName templateText
  liftEither $ left show templateOrError

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

sendSubscribedEmail :: Secrets -> Subscriber -> EitherIO ()
sendSubscribedEmail (Secrets {secretPostmarkKey, secretServerUrl}) subscriber = do
  template <- loadTemplate "subscribed.mustache"
  let (email, unsubUrl) = (subscriberEmail subscriber, getUnsubUrl secretServerUrl subscriber)
      compileData = Mustache.object [("unsubscribe_url", Mustache.String unsubUrl)]
      htmlText = Mustache.substitute template compileData
  sendEmail secretPostmarkKey email "This is a test!" htmlText "on-subscribe"
