module Halogen.SimpleButtonComponent where

-- | a trivially simple button where the toggled variant has a button with text
-- | that toggles each time it is pressed.
-- | The plain vanilla component has static text.
-- | Whether or not it is enabled may be set externally via a query

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { isOn :: Boolean
  , isEnabled :: Boolean
  }

data Query a =
    Toggle a
  | UpdateEnabled Boolean a

data Message = Toggled Boolean

-- | the basic component has a label on the button that never alters
component :: forall m. String -> H.Component HH.HTML Query Unit Message m
component label =
  toggledLabelComponent label label

-- | but the toggled label component toggles between labels each time the button is pressed
toggledLabelComponent :: forall m. String -> String -> H.Component HH.HTML Query Unit Message m
toggledLabelComponent offlabel onLabel =
  H.component
    { initialState: const initialState
    , render: render offlabel onLabel
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { isOn : false
    , isEnabled : true
    }

  render :: String -> String -> State -> H.ComponentHTML Query
  render offLabel onLabel state =
    let
      label =
        if (state.isOn) then
          onLabel
        else
          offLabel
    in
      HH.button
        [ HE.onClick (HE.input_ Toggle)
        , HP.class_ $ ClassName "hoverable"
        , HP.enabled state.isEnabled
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = state { isOn = not state.isOn }
      H.put nextState
      H.raise $ Toggled nextState.isOn
      pure next
    UpdateEnabled isEnabled next -> do
      _ <- H.modify (\state -> state {isEnabled = isEnabled})
      pure next
