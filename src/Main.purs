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
    -- liftEff' $ play ctx 3.0 example1
    liftEff' $ play ctx 2.0 example3

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
  "Gain id1 { gain [ setValue 0.1, exponentialRampToValueAtTime 2 2.0 ] } [ output ] " <>
  "Oscillator id2 { type sawtooth frequency 240 } [ id1 ] " <>
  "End"

{-
now <- currentTime ctx
  -- we make the basic bell sound from a couple of frequencies
  -- the first oscillator
  osc1 <- createOscillator ctx
  _ <- setOscillatorType Square osc1
  _ <- setFrequency 800.0 osc1

  -- the second oscillator
  osc2 <- createOscillator ctx
  _ <- setOscillatorType Square osc2
  _ <- setFrequency 540.0 osc2

  -- the gain
  gainNode <- createGain ctx
  gainParam <- gain gainNode
  _ <- setValue 0.5 gainParam
  -- set the start gain
  _ <- setValueAtTime 0.5 now gainParam
  -- gradually reduce the amplitude to a minimum value of 0.01
  _ <- exponentialRampToValueAtTime 0.01 (now + 1.0) gainParam

  -- the filter
  filter <- createBiquadFilter ctx
  _ <- setFilterType Bandpass filter
  -- the bandpass filter concentrates on passing through a narrow band of
  -- frequencies centered around the one supplied, attenuating the others
  freqParam <- filterFrequency filter
  _ <- setValue 800.0 freqParam

  dst <- destination ctx

  -- connect it all up
  _ <- connect osc1 gainNode
  _ <- connect osc2 gainNode
  _ <- connect gainNode filter
  _ <- connect filter dst
-}

-- | cowbell
example3 :: String
example3 =
  "BiquadFilter filter1 { type bandpass frequency 800 } [ output ] " <>
  "Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 0, exponentialRampToValueAtTime 0.01 1.0 ] } [ filter1 ] " <>
  "Oscillator osc1 { type square frequency 540 } [ gain1 ]  " <>
  "Oscillator osc2 { type square frequency 800 } [ gain1 ] " <>
  "End"
