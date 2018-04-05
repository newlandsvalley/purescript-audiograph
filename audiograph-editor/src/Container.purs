module Container where

import Prelude


import Audio.WebAudio.Types (WebAudio, AudioContext)
import Audio.Graph (AudioGraph, Assemblage)
import Audio.Graph.Control (start, stop) as Control
import Audio.Graph.Builder (build)
import Control.Monad.Aff (Aff, liftEff')
import Data.Either (Either(..), either)
import Data.Either.Nested (Either6)
import Audio.Graph.Parser  (PositionedParseError(..))
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.SimpleButtonComponent as Button
import JS.FileIO (FILEIO, Filespec, saveTextFile)
import Network.HTTP.Affjax (AJAX)
import SampleText (cowbell, frequencyModulation)


type AppEffects eff = (ajax :: AJAX, wau :: WebAudio, fileio :: FILEIO | eff)

type State =
  { ctx :: AudioContext
  , graphResult :: Either PositionedParseError AudioGraph
  , assemblage :: Maybe Assemblage
  , fileName :: Maybe String
  }

data Query a =
    HandleAUGFile FIC.Message a
  | HandleClearButton Button.Message a
  | HandleSaveButton Button.Message a
  | HandleSampleButton Button.Message a
  | HandlePlayButton Button.Message a
  | HandleNewAudioGraphText ED.Message a

augFileInputCtx :: FIC.Context
augFileInputCtx =
  { componentId : "graphinput"
  , isBinary : false
  , prompt : "choose"
  , accept : MediaType ".aug, .txt"
  }

-- | there is no audio graph yet
nullGraph :: Either PositionedParseError AudioGraph
nullGraph =
  Left (PositionedParseError { pos : 0, error : "" })

parseError :: Either PositionedParseError AudioGraph -> String
parseError graphResult =
  case graphResult of
    Right _ -> "no errors"
    Left (PositionedParseError ppe) -> "parse error: " <> ppe.error

-- type ChildQuery = Coproduct6 ED.Query FIC.Query Button.Query Button.Query Button.Query PC.Query
type ChildQuery = Coproduct6 ED.Query FIC.Query Button.Query Button.Query Button.Query Button.Query

-- slots and slot numbers
type FileInputSlot = Unit
type PlayerSlot = Unit
type ReplaceInstrumentsSlot = Unit
type ClearTextSlot = Unit
type SaveTextSlot = Unit
type SampleTextSlot = Unit
type EditorSlot = Unit

type ChildSlot = Either6 Unit Unit Unit Unit Unit Unit

editorSlotNo :: CP.ChildPath ED.Query ChildQuery EditorSlot ChildSlot
editorSlotNo = CP.cp1

psomFileSlotNo :: CP.ChildPath FIC.Query ChildQuery FileInputSlot ChildSlot
psomFileSlotNo = CP.cp2

clearTextSlotNo :: CP.ChildPath Button.Query ChildQuery ClearTextSlot ChildSlot
clearTextSlotNo = CP.cp3

saveTextSlotNo :: CP.ChildPath Button.Query ChildQuery SaveTextSlot ChildSlot
saveTextSlotNo = CP.cp4

sampleTextSlotNo :: CP.ChildPath Button.Query ChildQuery SampleTextSlot ChildSlot
sampleTextSlotNo = CP.cp5

-- playerSlotNo :: CP.ChildPath PC.Query ChildQuery PlayerSlot ChildSlot
playerSlotNo :: CP.ChildPath Button.Query ChildQuery PlayerSlot ChildSlot
playerSlotNo = CP.cp6


component ::  ∀ eff. AudioContext -> H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
-- component ::  ∀ eff p. H.Component HH.HTML Query Unit Void (Aff (au :: AUDIO, fileio :: FILEIO, sdom :: SDOM | eff))
component ctx =
  H.parentComponent
    { initialState: const (initialState ctx)
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: AudioContext -> State
  initialState ctx =
    { ctx : ctx
    , graphResult: nullGraph
    , assemblage : Nothing
    , fileName: Nothing
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  render state = HH.div_
    [ HH.h1
      [HP.class_ (H.ClassName "center") ]
      [HH.text "PureScript AudioGraph Editor"]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent")  ]
         [ HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "load audiograph file:" ]
         , HH.slot' psomFileSlotNo unit (FIC.component augFileInputCtx) unit (HE.input HandleAUGFile)
         , HH.slot' sampleTextSlotNo unit (Button.component "example") unit (HE.input HandleSampleButton)
         ]
      ,  HH.div
          -- save
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "save or clear:" ]
          , HH.slot' saveTextSlotNo unit (Button.component "save") unit (HE.input HandleSaveButton)
          -- clear
          , HH.slot' clearTextSlotNo unit (Button.component "clear") unit (HE.input HandleClearButton)
          ]
      ,  renderPlayButton state
      ,  debug state
      ]
      -- right pane - editor
      , HH.div
          [ HP.class_ (H.ClassName "rightPane") ]
          [
            HH.slot' editorSlotNo unit ED.component unit (HE.input HandleNewAudioGraphText)
          ]
    ]

  renderPlayButton ::  ∀ eff1. State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (wau :: WebAudio | eff1))
  renderPlayButton state =
    case state.graphResult of
      Right audioGraph ->
        let
          buttonText = maybe "play" (\_ -> "stop") state.assemblage
        in
          HH.div
            [ HP.class_ (H.ClassName "leftPanelComponent")]
            -- [ HH.slot' playerSlotNo unit (PC.component (PlayablePSoM psom) []) unit absurd  ]
            [ HH.slot' playerSlotNo unit (Button.component buttonText) unit (HE.input HandlePlayButton)]
      Left err ->
        HH.div_
          [  ]

  debug :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  debug state =
    case state.assemblage of
      Just ass ->
        HH.div_
          [HH.text "assemblage present"]
      _ ->
        HH.div_
          [HH.text "assemblage absent"]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff))
  eval (HandleAUGFile (FIC.FileLoaded filespec) next) = do
    H.modify (\st -> st { fileName = Just filespec.name } )
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent filespec.contents)
    pure next
  eval (HandleClearButton (Button.Toggled _) next) = do
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent "")
    pure next
  eval (HandleSaveButton (Button.Toggled _) next) = do
    maybeText <- H.query' editorSlotNo unit (H.request ED.GetText)
    state <- H.get
    let
      fileName = getFileName state
      text = fromMaybe "" maybeText
      fsp = { name: fileName, contents : text} :: Filespec
    _ <- H.liftEff $ saveTextFile fsp
    pure next
  eval (HandleSampleButton (Button.Toggled _) next) = do
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent frequencyModulation)
    pure next
  eval (HandleNewAudioGraphText (ED.AudioGraphResult r) next) = do
    H.modify (\st -> st { graphResult = r} )
    pure next
  eval (HandlePlayButton (Button.Toggled _) next) = do
    state <- H.get
    newState <- H.liftAff $ togglePlayStop state
    H.put newState
    pure next


-- helpers
-- | get the file name
getFileName :: State -> String
getFileName state =
  case state.fileName of
    Just name ->
      name
    _ ->
      "untitled.aug"

-- toggle between playing the assembled audiograph and stopping it
togglePlayStop :: forall eff. State
      -> Aff (ajax :: AJAX, wau :: WebAudio | eff) State
togglePlayStop state =
  case state.assemblage of
    Just ass ->
      stop state
    _ ->
      play state


play :: forall eff. State
      -> Aff (ajax :: AJAX, wau :: WebAudio | eff) State
play state =
  case state.graphResult of
    Right audioGraph ->
      do
        -- build the web-audio assemblage
        assemblage <- build state.ctx audioGraph
        -- calculate the new state
        let
          newState = either (\err -> state) (\ass -> state { assemblage = Just ass }) assemblage
        -- play it if we can
        _ <- liftEff' $ either (\err -> pure unit) (Control.start 0.0) assemblage
        pure newState
    Left err ->
      pure $ state { assemblage = Nothing }

stop :: forall eff. State
      -> Aff (ajax :: AJAX, wau :: WebAudio | eff) State
stop state =
  case state.assemblage of
    Just ass ->
      do
        _ <- liftEff' $ Control.stop 0.0 ass
        pure $ state { assemblage = Nothing }
    _ ->
      pure state
