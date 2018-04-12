module SampleText (audioBuffer, cowbell, frequencyModulation, randomSample) where

import Prelude ((<>), ($), (-), bind, pure)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff (Eff)
import Data.Array ((!!), length)
import Data.Maybe (fromMaybe)

-- | cowbell
cowbell :: String
cowbell =
  "Oscillator osc2 { type square, frequency 800 } [ gain1 ] \n" <>
  "Oscillator osc1 { type square, frequency 540 } [ gain1 ]  \n" <>
  "Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 t + 0, exponentialRampToValueAtTime 0.01 t + 1.0 ] } [ filter ] \n" <>
  "BiquadFilter filter { type bandpass, frequency 800 } [ output ] \n" <>
  "End"

frequencyModulation :: String
frequencyModulation =
  "Oscillator modulator { frequency 0.8 } [ gain1 ] \n" <>
  "Oscillator carrier { frequency 300.0 } [ output ] \n" <>
  "Gain gain1 { gain 30.0 } [ carrier.frequency ] \n" <>
  "End"

-- | audio buffer source
audioBuffer :: String
audioBuffer =
  "Gain id1 { gain 2 } [ output ] \n" <>
  "AudioBufferSource id2 { url https://raw.githubusercontent.com/borismus/webaudioapi.com/master/content/posts/audio-tag/chrono.mp3 \n" <>
                         ", loop true}  [ id1 ] \n" <>
  "End"

samples :: Array String
samples =
  [ cowbell
  , frequencyModulation
  , audioBuffer
  ]

randomSample :: ∀ eff. Eff (random :: RANDOM | eff) String
randomSample = do
  idx <- randomInt 0 (length samples - 1)
  pure $ fromMaybe cowbell $ samples !! idx
