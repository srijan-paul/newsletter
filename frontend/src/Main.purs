module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Flame (Html, QuerySelector(..), Subscription)
import Flame.Application.Effectful as FA
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Subscription.Window (onLoad)
import Flame.Types (NodeData)

type Model =
  { emailSubject :: Maybe String
  , emailBodyMd :: Maybe String
  , authToken :: Maybe String
  }

data Message = SendEmail | TextChanged | CreateEditor

-- | Initial state of the app
init :: Model /\ (Maybe Message)
init =
  { emailSubject: Nothing
  , emailBodyMd: Nothing
  , authToken: Nothing
  } /\ Nothing

placeHolderText :: String
placeHolderText =
  """# This is an Email.
Written in *markdown*!"""

-- | `update` is called to handle events
update :: FA.AffUpdate Model Message
update { message: CreateEditor, model } = do
  pure $ const model
update { model } = pure $ const model

-- | `view` is called whenever the model is updated
view :: Model -> Html Message
view _ = HE.main "main"
  [ HE.div
      [ HA.id "editor", HA.style [ ("width" /\ "50%"), ("height" /\ "400px") ] ]
      [ HE.textarea' textAreaAttrs ]
  ]
  where
  textAreaAttrs :: Array (NodeData Message)
  textAreaAttrs =
    [ HA.id "markdownInput"
    , HA.placeholder "Write your email in markdown..."
    , HA.rows 10
    , HA.cols 50
    ]

-- | Events that come from outside the `view`
subscribe :: Array (Subscription Message)
subscribe = [ onLoad CreateEditor ]

-- | Mount the application on the given selector
main :: Effect Unit
main = do
  FA.mount_ (QuerySelector "body")
    { init: init
    , update: update
    , view: view
    , subscribe: subscribe
    }
