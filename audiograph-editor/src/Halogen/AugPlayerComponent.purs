module Halogen.AugPlayerComponent where

-- | An autonomoust audiograph player.
-- | This is a simple button toggled with play/stop which
-- | then plays or stops the playback of the audiograph
-- | it also exposes a Stop query, allowing it to be stopped
-- | by the calling program

-- | remember that web-audio doesn't allow you to stop and then restart
-- | an audio stream.  You must rebuild in between.

import Prelude


import Control.Monad.Aff (Aff, liftEff')
import Network.HTTP.Affjax (AJAX)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Either (either, hush)
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

type Input = AudioGraph

type State =
  { ctx :: AudioContext
  , audioGraph :: AudioGraph
  , assemblage :: Maybe Assemblage
  }

data Query a
  = Toggle a
  | Stop a
  | HandleInput AudioGraph a

data Message = Toggled Boolean


component :: forall eff. AudioContext -> AudioGraph -> H.Component HH.HTML Query Input Message (Aff (PlayerEffects eff))
component ctx audioGraph =
  H.component
    { initialState: const (initialState ctx audioGraph)
    , render: render
    , eval
    , receiver: HE.input HandleInput
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
    -- toggle between play and stop when the button is pressed
    Toggle next -> do
      state <- H.get
      newState <- H.liftAff $ togglePlayStop state
      H.put newState
      H.raise $ Toggled (isPlaying newState)
      pure next
    -- stop when requested externally
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
    -- stop then handle a new audiograph when requested externally
    HandleInput audioGraph next -> do
      state <- H.get
      newState <- H.liftAff $ stop state
      H.put newState { audioGraph = audioGraph }
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
-- at the moment, this throws away build errors
play :: forall eff. State
      -> Aff (PlayerEffects eff) State
play state = do
  assemblage <-
    if isNothing state.assemblage
      then
        -- build the web-audio assemblage
        hush <$> build state.ctx state.audioGraph
      else
        -- don't bother rebuilding
        pure state.assemblage
  let
    newState = state { assemblage = assemblage }
  -- play it if we can
  _ <- liftEff' $ maybe (pure unit) (Control.start 0.0) assemblage
  pure newState


{-}
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
-}

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
