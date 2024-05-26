module Email (sendSubscribedEmail) where

import Control.Arrow (ArrowChoice (left))
import Control.Monad.Except (ExceptT, liftEither, liftIO)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Db.Core (Subscriber (..))
import Network.HTTP.Simple
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

subscriberUnsubUrl :: Subscriber -> T.Text
subscriberUnsubUrl = ("http://localhost:3000/unsubscribe/" <>) . subscriberId

data PostmarkRequest = PostmarkRequest
  { from :: T.Text,
    to :: T.Text,
    subject :: T.Text,
    htmlBody :: T.Text,
    messageStream :: T.Text
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

sendEmail :: T.Text -> T.Text -> T.Text -> T.Text -> EitherIO ()
sendEmail to subject body stream =
  do
    req' <- parseRequest "POST https://api.postmarkapp.com/email"
    let headers =
          [ ("Content-Type", "application/json"),
            ("X-Postmark-Server-Token", "[REDACTED]"),
            ("Accept", "application/json")
          ]
        postMarkReq = PostmarkRequest "srijan@injuly.in" to subject body stream
        request = setRequestHeaders headers (setRequestBodyJSON postMarkReq req')
    response <- httpNoBody request
    print (getResponseStatusCode response)
    return ()

sendSubscribedEmail :: Subscriber -> EitherIO ()
sendSubscribedEmail subscriber = do
  template <- loadTemplate "subscribed.mustache"
  let (email, unsubUrl) = (subscriberEmail subscriber, subscriberUnsubUrl subscriber)
      compileData = Mustache.object [("unsubscribe_url", Mustache.String unsubUrl)]
      htmlText = Mustache.substitute template compileData
  sendEmail email "Testing!" htmlText "on-subscribe"
