module Audio.Graph.Control (start, stop, startThenStop) where

-- | control the starting and stopping of an audio assemblage
-- | this will eventually be extended to include modifications to
-- | a running assemblage (if we can)

import Prelude (Unit, bind, pure, unit)
import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Audio.Graph
import Audio.WebAudio.Types (WebAudio, AudioNode(..))
import Audio.WebAudio.Oscillator (startOscillator, stopOscillator)
import Audio.WebAudio.AudioBufferSourceNode (startBufferSource, stopBufferSource)


import Debug.Trace (trace)

-- | start to play an Assemblage at the given time offset
start :: ∀ eff. Number -> Assemblage -> Eff ( wau :: WebAudio | eff) Unit
start delay assemblage =
  traverse_ (startNode delay) assemblage

-- | stop playing an Assemblage after the supplied duration
stop :: ∀ eff. Number -> Assemblage -> Eff ( wau :: WebAudio | eff) Unit
stop delay assemblage =
  traverse_ (stopNode delay) assemblage

-- | start playing and then stop after the supplied duration
startThenStop :: ∀ eff. Number -> Number -> Assemblage
     -> Eff (wau :: WebAudio | eff) Unit
startThenStop startTime stopTime assemblage = do
  _ <- start startTime assemblage
  stop stopTime assemblage

startNode :: ∀ eff. Number -> AudioNode -> Eff ( wau :: WebAudio | eff) Unit
startNode delay node =
  case node of
    Oscillator osc ->
      trace "oscillator started" \_ ->
      startOscillator delay osc
    AudioBufferSource src ->
      trace "audio buffer source started" \_ ->
      startBufferSource delay src
    _ ->
      pure unit

stopNode :: ∀ eff. Number -> AudioNode -> Eff ( wau :: WebAudio | eff) Unit
stopNode delay node =
  case node of
    Oscillator osc ->
      trace "oscillator stopped" \_ ->
      stopOscillator delay osc
    AudioBufferSource src ->
      trace "audio buffer source stopped" \_ ->
      stopBufferSource delay src
    _ ->
      pure unit
