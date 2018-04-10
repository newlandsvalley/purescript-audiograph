module Halogen.AugPlayerComponent where

-- | An autonomoust audiograph player.
-- | This is a simple button toggled with play/stop which
-- | then plays or stops the playback of the audiograph
-- | it also exposes a Stop query, allowing it to be stopped
-- | by the calling program

import Prelude


import Control.Monad.Aff (Aff, liftEff')
import Network.HTTP.Affjax (AJAX)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Audio.Graph (AudioGraph, Assemblage)
import Audio.Graph.Control (start, stop) as Control
import Audio.Graph.Builder (build)
import Audio.WebAudio.Types (WebAudio, AudioContext)

type PlayerEffects eff = (ajax :: AJAX, wau :: WebAudio | eff)

type State =
  { ctx :: AudioContext
  , audioGraph :: AudioGraph
  , assemblage :: Maybe Assemblage
  }

data Query a
  = Toggle a
  | Stop a
  
data Message = Toggled Boolean


component :: forall eff. AudioContext -> AudioGraph -> H.Component HH.HTML Query Unit Message (Aff (PlayerEffects eff))
component ctx audioGraph =
  H.component
    { initialState: const (initialState ctx audioGraph)
    , render: render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: AudioContext -> AudioGraph -> State
  initialState ctx audioGraph =
    { ctx : ctx
    , audioGraph : audioGraph
    , assemblage : Nothing
    }

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label =
        if (isPlaying state) then
          "stop"
        else
          "play"
    in
      HH.button
        [ HE.onClick (HE.input_ Toggle)
        , HP.class_ $ ClassName "hoverable"
        ]
        [ HH.text label ]

  eval :: ∀ eff'. Query ~> H.ComponentDSL State Query Message (Aff (PlayerEffects eff'))
  eval = case _ of
    Toggle next -> do
      state <- H.get
      newState <- H.liftAff $ togglePlayStop state
      H.put newState
      H.raise $ Toggled (isPlaying newState)
      pure next
    Stop next -> do
      state <- H.get
      if (isPlaying state)
        then do
          newState <- H.liftAff $ stop state
          H.put newState
          H.raise $ Toggled (isPlaying newState)
          pure next
        else
          pure next

isPlaying :: State -> Boolean
isPlaying state =
  maybe false (\_ -> true) state.assemblage

-- toggle between playing the assembled audiograph and stopping it
togglePlayStop :: forall eff. State
      -> Aff (PlayerEffects eff) State
togglePlayStop state =
  case state.assemblage of
    Just ass ->
      stop state
    _ ->
      play state

-- play the audio graph if we can
play :: forall eff. State
      -> Aff (PlayerEffects eff) State
play state =
  do
    -- build the web-audio assemblage
    assemblage <- build state.ctx state.audioGraph
    -- calculate the new state
    let
      newState = either
          (\err -> state)
          (\ass -> state { assemblage = Just ass }) assemblage
      -- play it if we can
    _ <- liftEff' $ either (\err -> pure unit) (Control.start 0.0) assemblage
    pure newState

-- stop the playing
stop :: forall eff. State
      -> Aff (PlayerEffects eff) State
stop state =
  case state.assemblage of
    Just ass ->
      do
        _ <- liftEff' $ Control.stop 0.0 ass
        pure $ state { assemblage = Nothing }
    _ ->
      pure $ state { assemblage = Nothing }
