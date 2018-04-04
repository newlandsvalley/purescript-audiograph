module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (Aff, liftEff', launchAff, delay)
import Network.HTTP.Affjax (AJAX)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Data.Map (empty)
import Data.Time.Duration (Milliseconds(..))


import Audio.WebAudio.Types (WebAudio, AudioContext)
import Audio.WebAudio.AudioContext (makeAudioContext)
import Audio.Graph (AudioGraph, Assemblage)
import Audio.Graph.Compiler (compile, compileUpdate)
import Audio.Graph.Parser (PositionedParseError(..))
import Audio.Graph.Control (start, startThenStop, stop)
import Audio.Graph.Builder (build)
import Audio.Graph.Updater (update)


main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, wau :: WebAudio | e) Unit
main = do
    {- at the moment I can't play more than one sample
       do I need somehow to clean up the AudioContext before reuse?
    _ <- liftEff' $ play ctx 3.0 example1
    delay (Milliseconds $ 4000.0)
    -}
    ctx <- makeAudioContext
    -- _ <- launchAff $ play ctx 3.0 example5
    _ <- launchAff $ startThenUpdate ctx 2.0 example1 example1Update
    pure unit

play :: forall e. AudioContext -> Number -> String
     -> Aff (ajax :: AJAX, console :: CONSOLE, wau :: WebAudio | e) Unit
play ctx duration text =
  let
    audioGraph=
      compile text
  in
    case audioGraph of
      Left (PositionedParseError ppe) ->
        liftEff' $ log ("compile error: " <> ppe.error)
      Right graph ->
        do
          assemblage <- build ctx graph
          liftEff' $ either (\err -> log ("load error: " <> err)) (startThenStop 0.0 duration) assemblage

startThenUpdate :: forall e. AudioContext -> Number -> String -> String
     -> Aff (ajax :: AJAX, console :: CONSOLE, wau :: WebAudio | e) Unit
startThenUpdate ctx duration text updateText =
  let
    audioGraph=
      compile text
    updateGraph=
      compileUpdate updateText
  in
    case Tuple audioGraph updateGraph of
      Tuple (Left (PositionedParseError ppe) ) _ ->
        liftEff' $ log ("nodedef compile error: " <> ppe.error)
      Tuple _ (Left (PositionedParseError ppe) ) ->
        liftEff' $ log ("update parse error: " <> ppe.error)
      Tuple (Right graph) (Right graphChange) ->
        do
          eassemblage <- build ctx graph
          either (\err -> liftEff' $ log ("load error: " <> err))
                 (\assemblage -> updateSequence duration assemblage graphChange) eassemblage

updateSequence :: ∀ eff. Number -> Assemblage -> AudioGraph -> Aff (wau :: WebAudio | eff) Unit
updateSequence duration assemblage graphChange = do
  _ <- liftEff' $ start 0.0 assemblage
  _ <- delay (Milliseconds 1000.0)
  _ <- liftEff' $update empty assemblage graphChange
  liftEff' $ stop duration assemblage


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
  "Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 0, exponentialRampToValueAtTime 0.01 1.0 ] } [ filter1 ] " <>
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
