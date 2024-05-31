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

emailBox :: Html Message
emailBox = HE.div [ HA.class' "email-container" ] [ bodyInput ]
  where
  bodyInput = HE.div [ HA.class' "email-body" ] [ HE.textarea' textAreaAttrs ]

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
        ]
    ]

-- | `view` is called whenever the model is updated
view :: Model -> Html Message
view _ = HE.main "main" [ mailEditor ]
  where
  mailEditor = HE.div [ HA.class' "mail-container" ]
    [ subjectContainer, editorContainer ]

  subjectContainer = HE.div
    [ HA.class' "email-subject-container" ]
    [ HE.input [ HA.placeholder "Subject line", HA.type' "text", HA.class' "subject" ]
    , HE.button [ HA.class' "send-button", HA.onClick SendEmail ] "Send"
    ]

  editorContainer = HE.div
    [ HA.class' "editor-and-preview-container" ]
    [ emailBox, preview ]

  preview = HE.div
    [ HA.id "preview" ]
    [ HE.div [ HA.class' "preview-default" ] "Preview will be shown here" ]

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
