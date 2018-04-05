module SampleText (cowbell) where

import Prelude ((<>))

-- | cowbell
cowbell :: String
cowbell =
  "Oscillator osc2 { type square, frequency 800 } [ gain1 ] \n" <>
  "Oscillator osc1 { type square, frequency 540 } [ gain1 ]  \n" <>
  "Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 0, exponentialRampToValueAtTime 0.01 1.0 ] } [ filter1 ] \n" <>
  "BiquadFilter filter1 { type bandpass, frequency 800 } [ output ] \n" <>
  "End"
