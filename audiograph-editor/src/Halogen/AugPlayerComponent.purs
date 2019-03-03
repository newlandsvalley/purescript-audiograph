module Halogen.AugPlayerComponent where

-- | An autonomoust audiograph player.
-- | This is a simple button toggled with play/stop which
-- | then plays or stops the playback of the audiograph
-- | it also exposes a Stop query, allowing it to be stopped
-- | by the calling program

-- | remember that web-audio doesn't allow you to stop and then restart
-- | an audio stream.  You must rebuild in between.

-- | The player can be in the following states defined by Either String Assemblage
-- | not started : Left ""
-- | playing : Right assemblage
-- | not playing because of a build error: left "error message"

import Prelude


import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Data.Either (Either(..), either, isLeft)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Audio.Graph (AudioGraph, Assemblage)
import Audio.Graph.Control (start, stop) as Control
import Audio.Graph.Builder (build)
import Audio.WebAudio.Types (AudioContext)

type Slot = H.Slot Query Message

-- type Input = AudioGraph -- JMW!!!

type State =
  { ctx :: AudioContext
  , audioGraph :: AudioGraph
  , assemblage :: Either String Assemblage
  }

-- actions are those that derive from HTML events
data Action =
    ToggleAction
  | StopAction

data Query a
  = Toggle a
  | Stop a
  | HandleInput AudioGraph a

data Message = Toggled Boolean

component :: AudioContext -> AudioGraph -> H.Component HH.HTML Query Unit Message Aff
component ctx audioGraph  =
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

  initialState :: ∀ i. i -> State
  initialState _  =
    { ctx : ctx
    , audioGraph : audioGraph
    , assemblage : Left ""
    }

  render :: State -> H.ComponentHTML Action () Aff
  render state =
    let
      label =
        if (isPlaying state) then
          "stop"
        else
          "play"
    in
      HH.div_
        [ HH.button
            [ HE.onClick (\_ -> Just ToggleAction)
            , HP.class_ $ ClassName "hoverable"
            ]
            [ HH.text label ]
          -- display any build error when trying to uild the assemblage from the graph
          , HH.text (buildError state)
        ]

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery = case _ of
  -- toggle between play and stop when the button is pressed
  Toggle next -> do
    state <- H.get
    newState <- H.liftAff $ togglePlayStop state
    H.put newState
    H.raise $ Toggled (isPlaying newState)
    pure (Just next)
  -- stop when requested externally
  Stop next -> do
    state <- H.get
    if (isPlaying state)
      then do
        newState <- H.liftAff $ stop state
        H.put newState
        H.raise $ Toggled (isPlaying newState)
        pure (Just next)
      else
        pure (Just next)
  HandleInput audioGraph next -> do
    state <- H.get
    newState <- H.liftAff $ stop state
    H.put newState { audioGraph = audioGraph }
    pure (Just next)

-- handling an action from HTML events just delegates to the appropriate query
-- I'm not sure why using unit is kosher for  the query's a param here but it
-- seems OK.
handleAction ∷ Action → H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
  StopAction -> do
    _ <- handleQuery (Stop unit)
    pure unit
  ToggleAction -> do
    _ <- handleQuery (Toggle unit)
    pure unit

isPlaying :: State -> Boolean
isPlaying state =
  either (\_ -> false) (\_ -> true) state.assemblage

-- toggle between playing the assembled audiograph and stopping it
togglePlayStop :: State -> Aff State
togglePlayStop state =
  case state.assemblage of
    Right ass ->
      stop state
    _ ->
      play state

-- play the audio graph if we can
-- at the moment, this throws away build errors
play :: State -> Aff State
play state = do
  assemblage <-
    if isLeft state.assemblage
      then
        -- build the web-audio assemblage
        build state.ctx state.audioGraph
      else
        -- don't bother rebuilding
        pure state.assemblage
  let
    newState = state { assemblage = assemblage }
  -- play it if we can
  _ <- liftEffect $ either (\err -> pure unit) (Control.start 0.0) assemblage
  pure newState

-- stop the playing
stop :: State -> Aff State
stop state =
  case state.assemblage of
    Right ass ->
      do
        _ <- liftEffect $ Control.stop 0.0 ass
        pure $ state { assemblage = Left "" }
    _ ->
      pure $ state { assemblage = Left "" }

buildError :: State -> String
buildError state =
  either identity (\_ -> "") state.assemblage
