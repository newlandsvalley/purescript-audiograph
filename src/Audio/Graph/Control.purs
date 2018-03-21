module Audio.Graph.Control (start, stop) where

-- | control the starting and stopping of an audio assemblage
-- | this will eventually be extended to include modifications to
-- | a running assemblage (if we can)

import Prelude (Unit, pure, unit)
import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Audio.Graph
import Audio.WebAudio.Types (WebAudio, AudioNode(..))
import Audio.WebAudio.Oscillator (startOscillator, stopOscillator)

start :: ∀ eff. Number -> Assemblage -> Eff ( wau :: WebAudio | eff) Unit
start delay assemblage =
  traverse_ (startNode delay) assemblage

startNode :: ∀ eff. Number -> AudioNode -> Eff ( wau :: WebAudio | eff) Unit
startNode delay node =
  case node of
    Oscillator osc ->
      startOscillator delay osc
    _ ->
      pure unit

stop :: ∀ eff. Number -> Assemblage -> Eff ( wau :: WebAudio | eff) Unit
stop delay assemblage =
  traverse_ (stopNode delay) assemblage

stopNode :: ∀ eff. Number -> AudioNode -> Eff ( wau :: WebAudio | eff) Unit
stopNode delay node =
  case node of
    Oscillator osc ->
      stopOscillator delay osc
    _ ->
      pure unit
