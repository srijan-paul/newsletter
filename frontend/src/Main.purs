module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Flame (Html, QuerySelector(..), Subscription)
import Flame.Application.Effectful as FA
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Markdown (renderMarkdownToDiv)

type Model =
  { emailSubject :: Maybe String
  , emailBodyHtml :: Maybe String
  , authToken :: Maybe String
  }

data Message
  = SendEmail
  | TextInput String

-- | Initial state of the app
init :: Model /\ (Maybe Message)
init =
  { emailSubject: Nothing
  , emailBodyHtml: Nothing
  , authToken: Nothing
  } /\ Nothing

-- | `update` is called to handle events
update :: FA.AffUpdate Model Message
update { message: (TextInput emailContent), model } = do
  liftEffect $ renderMarkdownToDiv emailContent "preview"
  pure $ const model
update { model } = pure $ const model

-- | `view` is called whenever the model is updated
view :: Model -> Html Message
view _ = HE.main "main"
  [ editorWithPreview, HE.div [HA.style [("margin-left" /\ "2em")] ] [sendButton] ]

  where
  sendButton = HE.button
    [ HA.class' "button", HA.style [ ("margin-top" /\ "20px") ] ]
    "Send Mail"

  editorWithPreview = HE.div
    [ HA.class' "editor-container" ]
    [ textArea, preview ]

  preview = HE.div' [ HA.id "preview" ]
  textArea = HE.div [ HA.class' "email-input" ] [ HE.textarea' textAreaAttrs ]

  textAreaAttrs :: Array (NodeData Message)
  textAreaAttrs =
    [ HA.id "markdownInput"
    , HA.placeholder "Write your mail in markdown"
    , HA.onInput TextInput
    , HA.style
        [ ("width" /\ "100%")
        , ("height" /\ "100%")
        , ("padding" /\ "0")
        , ("resize" /\ "none")
        , ("border" /\ "none")
        , ("outline" /\ "none")
        , ("overflow" /\ "auto")
        , ("font-size" /\ "1.2em")
        ]
    ]

-- | Events that come from outside the `view`
subscribe :: Array (Subscription Message)
subscribe = []

-- | Mount the application on the given selector
main :: Effect Unit
main = do
  FA.mount_ (QuerySelector "body")
    { init: init
    , update: update
    , view: view
    , subscribe: subscribe
    }
