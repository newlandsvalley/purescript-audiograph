module Halogen.EditorComponent where

import Prelude

import Data.Either (Either, either)
import Data.String (fromCharArray, toCharArray, null) as S
import Data.Array (length, slice) as A
import Audio.Graph (AudioGraph)
import Audio.Graph.Parser (PositionedParseError(..))
import Audio.Graph.Compiler (compile)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen (IProp)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.CSS (style)
import CSS (color)
import Color (rgb)

type State =
  { text :: String
  , parseError :: Maybe PositionedParseError
  }

data Query a =
    UpdateContent String a
  | GetText (String -> a)

data Message = AudioGraphResult (Either PositionedParseError AudioGraph)

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { text : ""
    , parseError : Nothing
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.textarea
         [ HP.rows 15
         , HP.cols 70
         , HP.autofocus true
         , HP.value state.text
         , HP.class_ $ ClassName "audioGraphEdit"
         -- , HP.wrap false
         , HE.onValueInput (HE.input UpdateContent)
         ]
      , renderParseError state
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    UpdateContent s next -> do
      let
        audioGraphResult = compile s
        parseError = either Just (\success -> Nothing) audioGraphResult
      H.modify (\state -> state {text = s, parseError = parseError})
      H.raise $ AudioGraphResult audioGraphResult
      pure next
    GetText reply -> do
      state <- H.get
      pure (reply state.text)

renderParseError :: State -> H.ComponentHTML Query
renderParseError state =
  let
    -- the range of characters to display around each side of the error position
    textRange = 10
    txt = S.toCharArray state.text
  in
    case state.parseError of
      Just (PositionedParseError pe) ->
        if (S.null state.text) then
          HH.div_ []
        else
          let
            -- display a prefix of 5 characters before the error (if they're there) and a suffix of 5 after
            startPhrase =
              max (pe.pos - textRange) 0
            errorPrefix =
              A.slice startPhrase pe.pos txt
            startSuffix =
              min (pe.pos + 1) (A.length txt)
            endSuffix =
              min (pe.pos + textRange + 1) (A.length txt)
            errorSuffix =
              A.slice startSuffix endSuffix txt
            errorChar =
              A.slice pe.pos (pe.pos + 1) txt
          in
            HH.p_
              [ HH.text $ pe.error <> " - "
              , HH.text $ S.fromCharArray errorPrefix
              , HH.span
                 [ errorHighlightStyle ]
                 [ HH.text (S.fromCharArray errorChar) ]
              , HH.text $ S.fromCharArray errorSuffix
              ]
      _ ->
        HH.div_ []


errorHighlightStyle :: ∀ i r. IProp (style :: String | r) i
errorHighlightStyle =
  style do
    color $ (rgb 255 0 0)
