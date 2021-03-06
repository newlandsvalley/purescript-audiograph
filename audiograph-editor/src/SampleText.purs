module SampleText (audioBuffer, cowbell, frequencyModulation, randomSample) where

import Prelude ((<>), ($), (-), bind, pure)
import Effect.Random (randomInt)
import Effect (Effect)
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

feedback :: String
feedback =
  "AudioBufferSource abs { url assets/ogg/chop.ogg, loop true}  [ delay, output ] \n" <>
  "Delay delay { delayTime 0.2 } [ feedback, output ] \n" <>
  "Gain feedback { gain 0.8 } [ delay ] \n" <>
  "End"

stereoPan :: String
stereoPan =
  "AudioBufferSource abs { url assets/wav/pinknoise.wav, loop true}  [ panner ] \n" <>
  "StereoPanner panner { pan \n" <>
  "    [ setValueAtTime -1.0  t + 0.1, \n" <>
  "      linearRampToValueAtTime 1.0 t +10 \n" <>
  "    ] } [ gain ] \n" <>
  "Gain gain { gain 0.5 } [ output ] \n" <>
  "End"

dynamicsCompression :: String
dynamicsCompression =
  "AudioBufferSource abs { url assets/wav/pinknoise.wav, loop true}  [ compressor ] \n" <>
  "DynamicsCompressor compressor { \n" <>
  "  threshold  [ setValueAtTime -50  t + 2  ] , \n" <>
  "  knee  [ setValueAtTime 40  t + 2  ] , \n" <>
  "  ratio  [ setValueAtTime 12  t + 2  ] , \n" <>
  "  attack  [ setValueAtTime 0  t + 2  ] , \n" <>
  "  release  [ setValueAtTime 0.25  t + 2  ] \n" <>
  "} [ gain ] \n" <>
  "Gain gain { gain 0.5 } [ output ] \n" <>
  "End"

convolver :: String
convolver =
  "AudioBufferSource id2 { url https://raw.githubusercontent.com/borismus/webaudioapi.com/master/content/posts/audio-tag/chrono.mp3 \n" <>
  ", loop true}  [ gain ] \n" <>
  "Gain gain { gain 2 } [ conv ] \n" <>
  "Convolver conv { url assets/ogg/irHall.ogg } [ output ] \n" <>
  "End"

samples :: Array String
samples =
  [ cowbell
  , frequencyModulation
  , audioBuffer
  , feedback
  , stereoPan
  , dynamicsCompression
  , convolver
  ]

randomSample :: Effect String
randomSample = do
  idx <- randomInt 0 (length samples - 1)
  pure $ fromMaybe cowbell $ samples !! idx
