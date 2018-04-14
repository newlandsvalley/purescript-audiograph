module Container where

import Prelude


import Audio.WebAudio.Types (WebAudio, AudioContext)
import Audio.Graph (AudioGraph)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..), either)
import Data.Either.Nested (Either6)
import Audio.Graph.Parser  (PositionedParseError(..))
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.SimpleButtonComponent as Button
import Halogen.AugPlayerComponent as Player
import JS.FileIO (FILEIO, Filespec, saveTextFile)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Random (RANDOM)
import SampleText (randomSample)


type AppEffects eff = (ajax :: AJAX, wau :: WebAudio, fileio :: FILEIO, random :: RANDOM | eff)

type State =
  { ctx :: AudioContext
  , graphResult :: Either PositionedParseError AudioGraph
  , fileName :: Maybe String
  }

data Query a =
    HandleAugFile FIC.Message a
  | HandleClearButton Button.Message a
  | HandleSaveButton Button.Message a
  | HandleSampleButton Button.Message a
  | HandleAugPlayer Player.Message a
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
type ChildQuery = Coproduct6 ED.Query FIC.Query Button.Query Button.Query Button.Query Player.Query

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
playerSlotNo :: CP.ChildPath Player.Query ChildQuery PlayerSlot ChildSlot
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
    , fileName: Nothing
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
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
         , HH.slot' psomFileSlotNo unit (FIC.component augFileInputCtx) unit (HE.input HandleAugFile)
         , HH.slot' sampleTextSlotNo unit (Button.component "random") unit (HE.input HandleSampleButton)
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
      ]
      -- right pane - editor
      , HH.div
          [ HP.class_ (H.ClassName "rightPane") ]
          [
            HH.slot' editorSlotNo unit ED.component unit (HE.input HandleNewAudioGraphText)
          ]
    ]

  -- the play button is visible if we can parse the audiograph
  renderPlayButton ::  ∀ eff1. State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AJAX, wau :: WebAudio | eff1))
  renderPlayButton state =
    case state.graphResult of
      Right audioGraph ->
        HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.slot' playerSlotNo unit (Player.component state.ctx audioGraph) audioGraph (HE.input HandleAugPlayer) ]
          -- [ HH.slot' playerSlotNo unit (Player.component state.ctx audioGraph) unit (HE.input HandleAugPlayer) ]
      Left err ->
        HH.div_
          [  ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff))
  eval (HandleAugFile (FIC.FileLoaded filespec) next) = do
    H.modify (\st -> st { fileName = Just filespec.name } )
    _ <- H.query' playerSlotNo unit $ H.action (Player.Stop)
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent filespec.contents)
    pure next
  eval (HandleClearButton (Button.Toggled _) next) = do
    H.modify (\st -> st { fileName = Nothing } )
    _ <- H.query' playerSlotNo unit $ H.action (Player.Stop)
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
    H.modify (\st -> st { fileName = Nothing } )
    sample <- H.liftEff randomSample
    _ <- H.query' playerSlotNo unit $ H.action (Player.Stop)
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent sample)
    pure next
  eval (HandleNewAudioGraphText (ED.AudioGraphResult r) next) = do
    _ <- refreshPlayerState r
    H.modify (\st -> st { graphResult = r} )
    pure next
  -- all the activity of the audiograph player is handed off to the player itself
  eval (HandleAugPlayer (Player.Toggled _) next) =
    pure next

-- refresh the state of the player
-- by passing it the audiograph result
refreshPlayerState :: ∀ eff.
       Either PositionedParseError AudioGraph
    -> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff)) Unit
refreshPlayerState audioGraphResult = do
  _ <- either
    (\_ -> H.query' playerSlotNo unit $ H.action (Player.Stop))
    (\graph -> H.query' playerSlotNo unit $ H.action (Player.HandleInput graph))
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
