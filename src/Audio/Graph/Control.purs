module Audio.Graph.Control (start, stop, startThenStop) where

-- | control the starting and stopping of an audio assemblage

import Prelude (Unit, bind, pure, unit)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Foldable (traverse_)
import Audio.Graph
import Audio.WebAudio.Types (AudioNode(..))
import Audio.WebAudio.Oscillator (startOscillator, stopOscillator)
import Audio.WebAudio.AudioBufferSourceNode (StartOptions, startBufferSource, stopBufferSource)


-- | start to play an Assemblage at the given time offset
start :: Number -> Assemblage -> Effect Unit
start delay assemblage =
  traverse_ (startNode delay) assemblage.nodes

-- | stop playing an Assemblage after the supplied duration
stop :: Number -> Assemblage -> Effect Unit
stop delay assemblage =
  traverse_ (stopNode delay) assemblage.nodes

-- | start playing and then stop after the supplied duration
startThenStop :: Number -> Number -> Assemblage
     -> Effect Unit
startThenStop startTime stopTime assemblage = do
  _ <- start startTime assemblage
  stop stopTime assemblage

startNode :: Number -> AudioNode -> Effect Unit
startNode delay node =
  case node of
    Oscillator osc ->
      -- trace "oscillator started" \_ ->
      startOscillator delay osc
    AudioBufferSource src ->
      let
        whenOption :: StartOptions
        whenOption = { when: Just delay,  offset: Nothing, duration: Nothing }
      in
        --trace "audio buffer source started" \_ ->
        startBufferSource whenOption src
    _ ->
      pure unit

stopNode :: Number -> AudioNode -> Effect Unit
stopNode delay node =
  case node of
    Oscillator osc ->
      -- trace "oscillator stopped" \_ ->
      stopOscillator delay osc
    AudioBufferSource src ->
      -- trace "audio buffer source stopped" \_ ->
      stopBufferSource delay src
    _ ->
      pure unit
