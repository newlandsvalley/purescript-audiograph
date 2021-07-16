module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff, delay)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Data.Map (empty)
import Data.Time.Duration (Milliseconds(..))


import Audio.WebAudio.Types (AudioContext)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Audio.Graph (AudioGraph, Assemblage)
import Audio.Graph.Compiler (compile, compileUpdate)
import Audio.Graph.Control (start, startThenStop, stop)
import Audio.Graph.Builder (build)
import Audio.Graph.Updater (update)


main :: Effect Unit
main = do
    ctx <- newAudioContext
    _ <- launchAff $ play ctx 3.0 example3
    -- _ <- launchAff $ startThenUpdate ctx 2.0 example1 example1Update
    pure unit

play :: AudioContext -> Number -> String -> Aff Unit
play ctx duration text =
  let
    audioGraph=
      compile text
  in
    case audioGraph of
      Left pe ->
        liftEffect $ log ("compile error: " <> pe.error)
      Right graph ->
        do
          assemblage <- build ctx graph
          liftEffect $ either (\err -> log ("load error: " <> err)) (startThenStop 0.0 duration) assemblage

startThenUpdate :: AudioContext -> Number -> String -> String -> Aff Unit
startThenUpdate ctx duration text updateText =
  let
    audioGraph=
      compile text
    updateGraph=
      compileUpdate updateText
  in
    case Tuple audioGraph updateGraph of
      Tuple (Left pe ) _ ->
        liftEffect $ log ("nodedef compile error: " <> pe.error)
      Tuple _ (Left pe ) ->
        liftEffect $ log ("update parse error: " <> pe.error)
      Tuple (Right graph) (Right graphChange) ->
        do
          eassemblage <- build ctx graph
          either (\err -> liftEffect $ log ("load error: " <> err))
                 (\assemblage -> updateSequence ctx duration assemblage graphChange) eassemblage

updateSequence :: AudioContext -> Number -> Assemblage -> AudioGraph -> Aff Unit
updateSequence ctx duration assemblage graphChange = do
  _ <- liftEffect $ start 0.0 assemblage
  _ <- delay (Milliseconds 1000.0)
  _ <- liftEffect $update ctx empty assemblage graphChange
  liftEffect $ stop duration assemblage


example1 :: String
example1 =
  "Gain id1 { gain 2 } [ output ] " <>
  "Oscillator id2 { type square, frequency 440 } [ id1 ] " <>
  "End"

example1Update :: String
example1Update =
  "Oscillator id2 { type square, frequency 880 } [ id1 ] " <>
  "End"

example2 :: String
example2 =
  "Gain id1 { gain [ setValue 0.1, exponentialRampToValueAtTime 2 2.0 ] } [ output ] " <>
  "Oscillator id2 { type sawtooth, frequency 240 } [ id1 ] " <>
  "End"

-- | cowbell
example3 :: String
example3 =
  "Oscillator osc2 { type square, frequency 800 } [ gain1 ] " <>
  "Oscillator osc1 { type square, frequency 540 } [ gain1 ]  " <>
  "Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 t + 0, exponentialRampToValueAtTime 0.01 t + 1.0 ] } [ filter1 ] " <>
  "BiquadFilter filter1 { type bandpass, frequency 800 } [ output ] " <>
  "End"

-- | audio buffer source
example4 :: String
example4 =
  "Gain id1 { gain 2 } [ output ]" <>
  "AudioBufferSource id2 { url wav/techno.wav, loop true}  [ id1 ]" <>
  "End"

-- | feedback
example5 :: String
example5 =
  "AudioBufferSource abs { url ogg/chop.ogg, loop true}  [ delay, output ]" <>
  "Delay delay { delayTime 0.5 } [ feedback, output ] " <>
  "Gain feedback { gain 0.8 } [ delay ]" <>
  "End"

-- frequency modulation
example6 :: String
example6 =
  "Oscillator modulator { frequency 0.8 } [ gain1 ]" <>
  "Oscillator carrier { frequency 300.0 } [ output ]" <>
  "Gain gain1 { gain 30.0 } [ carrier.frequency ] " <>
  "End"
