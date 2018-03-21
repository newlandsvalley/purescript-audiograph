module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (Aff, Fiber, delay, liftEff', launchAff)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))


import Audio.WebAudio.Types (WebAudio, AudioContext)
import Audio.WebAudio.AudioContext (makeAudioContext)
import Audio.Graph.Parser (parse)
import Audio.Graph.Assembler (assemble)
import Audio.Graph.Control (start, stop)

main :: forall e. Eff (console :: CONSOLE, wau :: WebAudio | e)
    (Fiber (console :: CONSOLE, wau :: WebAudio | e) Unit)
main = do
  ctx <- makeAudioContext
  launchAff $ do
    {- at the moment I can't play more than one sample
       do I need somehow to clean up the AudioContext before reuse?
    _ <- liftEff' $ play ctx 3.0 example1
    delay (Milliseconds $ 4000.0)
    -}
    liftEff' $ play ctx 3.0 example2

play :: forall e. AudioContext -> Number -> String -> Eff (console :: CONSOLE, wau :: WebAudio | e) Unit
play ctx duration text =
  let
    audioGraph=
      parse text
  in
    case audioGraph of
      Left err ->
        log ("parse error: " <> err)
      Right graph ->
        do
          assemblage <- assemble ctx graph
          start 0.0 assemblage
          stop duration assemblage


example1 :: String
example1 =
  "Gain id1 { gain 2 } [ output ] " <>
  "Oscillator id2 { type square frequency 440 } [ id1 ] " <>
  "End"

example2 :: String
example2 =
  "Gain id1 { gain [ setValue 0.1, linearRampToValueAtTime 2 2.0 ] } [ output ] " <>
  "Oscillator id2 { type sawtooth frequency 240 } [ id1 ] " <>
  "End"
