module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (Aff, Fiber, delay, liftEff', launchAff)
import Network.HTTP.Affjax (AJAX)
import Data.Either (Either(..), either)
import Data.Time.Duration (Milliseconds(..))


import Audio.WebAudio.Types (WebAudio, AudioContext)
import Audio.WebAudio.AudioContext (makeAudioContext)
import Audio.Graph.Compiler (compile)
import Audio.Graph.Control (startThenStop)
import Audio.Graph.Builder (build)


main :: forall e. Eff (ajax :: AJAX, console :: CONSOLE, wau :: WebAudio | e) Unit
main = do
    {- at the moment I can't play more than one sample
       do I need somehow to clean up the AudioContext before reuse?
    _ <- liftEff' $ play ctx 3.0 example1
    delay (Milliseconds $ 4000.0)
    -}
    ctx <- makeAudioContext
    _ <- launchAff $ play ctx 2.0 example4
    pure unit

play :: forall e. AudioContext -> Number -> String
     -> Aff (ajax :: AJAX, console :: CONSOLE, wau :: WebAudio | e) Unit
play ctx duration text =
  let
    audioGraph=
      compile text
  in
    case audioGraph of
      Left err ->
        liftEff' $ log ("parse error: " <> err)
      Right graph ->
        do
          assemblage <- build ctx graph
          liftEff' $ either (\err -> log ("load error: " <> err)) (startThenStop 0.0 duration) assemblage

example1 :: String
example1 =
  "Gain id1 { gain 2 } [ output ] " <>
  "Oscillator id2 { type square frequency 440 } [ id1 ] " <>
  "End"

example2 :: String
example2 =
  "Gain id1 { gain [ setValue 0.1, exponentialRampToValueAtTime 2 2.0 ] } [ output ] " <>
  "Oscillator id2 { type sawtooth frequency 240 } [ id1 ] " <>
  "End"

-- | cowbell
example3 :: String
example3 =
  "BiquadFilter filter1 { type bandpass frequency 800 } [ output ] " <>
  "Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 0, exponentialRampToValueAtTime 0.01 1.0 ] } [ filter1 ] " <>
  "Oscillator osc1 { type square frequency 540 } [ gain1 ]  " <>
  "Oscillator osc2 { type square frequency 800 } [ gain1 ] " <>
  "End"


{-}
  src <- createBufferSource ctx
  _ <- setLoop true src
  gain <- createGain ctx
  dst <- destination ctx
  _ <- connect src gain
  _ <- connect gain dst
  _ <- setBuffer buf src
  startTime <- currentTime ctx
  _ <- startBufferSource (startTime + 0.1) src
  pure { source : src, gain : gain}
-}

example4 :: String
example4 =
  "Gain id1 { gain 2 } [ output ]" <>
  "AudioBufferSource id2 { url wav/techno.wav loop true}  [ id1 ]" <>
  "End"
