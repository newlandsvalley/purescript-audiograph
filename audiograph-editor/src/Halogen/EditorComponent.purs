module Halogen.EditorComponent where

import Prelude

import Data.Either (Either, either)
import Data.String (null) as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Array (length, slice) as A
import Audio.Graph (AudioGraph)
import Audio.Graph.Parser (PositionedParseError(..))
import Audio.Graph.Compiler (compile)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.CSS (style)
import CSS (color)
import Color (rgb)

type State =
  { text :: String
  , parseError :: Maybe PositionedParseError
  }

type Slot = H.Slot Query Message

data Action = UpdateContentAction String

data Query a =
    UpdateContent String a
  | GetText (String -> a)

data Message = AudioGraphResult (Either PositionedParseError AudioGraph)

component :: ∀ i m. H.Component HH.HTML Query i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Nothing
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
    { text : ""
    , parseError : Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.textarea
         [ HP.rows 15
         , HP.cols 70
         , HP.autofocus true
         , HP.value state.text
         , HP.class_ $ ClassName "audioGraphEdit"
         -- , HP.wrap false
         , HE.onValueInput (Just <<< UpdateContentAction)
         ]
      , renderParseError state
      ]

handleAction ∷ ∀ m. Action → H.HalogenM State Action () Message m Unit
handleAction = case _ of
  UpdateContentAction s -> do
    -- delegate to the query
    _ <- handleQuery ((UpdateContent s) unit)
    pure unit

handleQuery :: ∀ a m. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  UpdateContent s next -> do
    let
      audioGraphResult = compile s
      parseError = either Just (\success -> Nothing) audioGraphResult
    _ <- H.modify (\state -> state {text = s, parseError = parseError})
    H.raise $ AudioGraphResult audioGraphResult
    pure (Just next)
  GetText reply -> do
    state <- H.get
    pure (Just (reply state.text))

renderParseError ::  ∀ m. State -> H.ComponentHTML Action () m
renderParseError state =
  let
    -- the range of characters to display around each side of the error position
    textRange = 10
    txt = toCharArray state.text
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
              , HH.text $ fromCharArray errorPrefix
              , HH.span
                 [ errorHighlightStyle ]
                 [ HH.text (fromCharArray errorChar) ]
              , HH.text $ fromCharArray errorSuffix
              ]
      _ ->
        HH.div_ []

errorHighlightStyle :: ∀ i r. HP.IProp (style :: String | r) i
errorHighlightStyle =
  style do
    color $ (rgb 255 0 0)
