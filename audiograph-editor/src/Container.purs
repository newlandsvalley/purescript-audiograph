module Container where

import Prelude

import Audio.WebAudio.Types (AudioContext)
import Audio.Graph (AudioGraph)
import Effect.Aff (Aff)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.SimpleButtonComponent as Button
import Halogen.AugPlayerComponent as PC
import JS.FileIO (Filespec, saveTextFile)
import SampleText (randomSample)
import Text.Parsing.StringParser (ParseError)
import Type.Proxy (Proxy(..))

type State =
  { ctx :: AudioContext
  , graphResult :: Either ParseError AudioGraph
  , fileName :: Maybe String
  }

data Action =
    HandleAugFile FIC.Message
  | HandleClearButton Button.Message
  | HandleSaveButton Button.Message
  | HandleSampleButton Button.Message
  | HandleAugPlayer PC.Message
  | HandleNewAudioGraphText ED.Message

augFileInputCtx :: FIC.Context
augFileInputCtx =
  { componentId : "graphinput"
  , isBinary : false
  , prompt : "choose"
  , accept : mediaType (MediaType ".aug, .txt")
  }

-- | there is no audio graph yet
nullGraph :: Either ParseError AudioGraph
nullGraph =
  Left { pos : 0, error : "" }

parseError :: Either ParseError AudioGraph -> String
parseError graphResult =
  case graphResult of
    Right _ -> "no errors"
    Left ppe -> "parse error: " <> ppe.error

type ChildSlots =
  ( editor :: ED.Slot Unit
  , augfile :: FIC.Slot Unit
  , clear :: Button.Slot Unit
  , savefile :: Button.Slot Unit
  , sample :: Button.Slot Unit
  , player :: PC.Slot Unit
  )

_editor = Proxy :: Proxy "editor"
_augfile = Proxy :: Proxy "augfile"
_clear = Proxy :: Proxy "clear"
_savefile = Proxy :: Proxy "savefile"
_sample = Proxy :: Proxy "sample"
_player = Proxy :: Proxy "player"

component :: forall q i o. AudioContext -> H.Component q i o Aff
component ctx =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction  }
    }
  where

  initialState :: i -> State
  initialState _ =
    { ctx : ctx
    , graphResult: nullGraph
    , fileName: Nothing
    }

  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state = HH.div_
    [ HH.h1
      [HP.class_ (H.ClassName "center") ]
      [HH.text "Web-Audio AudioGraph Editor"]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent")  ]
         [ HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "load aug file:" ]
         , HH.slot _augfile unit (FIC.component augFileInputCtx) unit HandleAugFile
         , HH.slot _sample unit (Button.component "example") unit HandleSampleButton
         ]
      ,  HH.div
          -- save
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "save or clear:" ]
          , HH.slot _savefile unit (Button.component "save") unit HandleSaveButton
          -- clear
          , HH.slot _clear unit (Button.component "clear") unit HandleClearButton
          ]
      ,  renderPlayButton state
      ]
      -- right pane - editor
      , HH.div
          [ HP.class_ (H.ClassName "rightPane") ]
          [
            HH.slot _editor unit ED.component unit HandleNewAudioGraphText
          ]
    ]

  -- the play button is visible if we can parse the audiograph
  renderPlayButton :: State -> H.ComponentHTML Action ChildSlots Aff
  renderPlayButton state =
    case state.graphResult of
      Right audioGraph ->
        HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [
            HH.slot _player unit (PC.component state.ctx audioGraph) unit HandleAugPlayer
          ]
      Left _ ->
        HH.div_
          [  ]

  handleAction :: Action → H.HalogenM State Action ChildSlots o Aff Unit
  handleAction = case _ of
    HandleAugFile (FIC.FileLoaded filespec) -> do
      _ <- H.modify (\st -> st { fileName = Just filespec.name } )
      _ <- H.tell _editor unit $ (ED.UpdateContent filespec.contents)
      _ <- H.tell _player unit PC.Stop
      pure unit
    HandleClearButton (Button.Toggled _) -> do
      _ <- H.modify (\st -> st { fileName = Nothing } )
      _ <- H.tell _editor unit $ (ED.UpdateContent "")
      _ <- H.tell _player unit PC.Stop
      pure unit
    HandleSaveButton (Button.Toggled _) -> do
      maybeText <- H.request _editor unit ED.GetText
      state <- H.get
      let
        fileName = getFileName state
        text = fromMaybe "" maybeText
        fsp = { name: fileName, contents : text} :: Filespec
      _ <- H.liftEffect $ saveTextFile fsp
      pure unit
    HandleSampleButton (Button.Toggled _) -> do
      _ <- H.modify (\st -> st { fileName = Nothing } )
      sample <- H.liftEffect randomSample
      _ <- H.tell _editor unit $ (ED.UpdateContent sample)
      _ <- H.tell _player unit PC.Stop
      pure unit
    HandleNewAudioGraphText (ED.AudioGraphResult r) -> do
      _ <- refreshPlayerState r
      _ <- H.modify (\st -> st { graphResult = r} )
      pure unit
    HandleAugPlayer (PC.Toggled _) -> do
      pure unit

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState :: ∀ o.
       Either ParseError AudioGraph
    -> H.HalogenM State Action ChildSlots o Aff Unit
refreshPlayerState audioGraphResult = do
  _ <- either
     (\_ -> H.tell _player unit PC.Stop)
     (\graph -> H.tell _player unit $ (PC.HandleInput graph))
     audioGraphResult
  pure unit

-- helpers
-- | get the file name
getFileName :: State -> String
getFileName state =
  case state.fileName of
    Just name ->
      name
    _ ->
      "untitled.aug"
