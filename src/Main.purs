module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))


import Audio.WebAudio.Types (WebAudio)
import Audio.Graph.Parser (parse)
import Audio.Graph.Assembler (assemble)
import Audio.Graph.Control (start, stop)

main :: forall e. Eff (console :: CONSOLE, wau :: WebAudio | e) Unit
main =
  let
    audioGraph=
      parse example2
  in
    case audioGraph of
      Left err ->
        log ("parse error: " <> err)
      Right graph ->
        do
          assemblage <- assemble graph
          start 0.0 assemblage
          stop 4.0 assemblage

example1 :: String
example1 =
  "Gain id1 { gain 2 } [ output ] " <>
  "Oscillator id2 { type square frequency 440 } [ id1 ] " <>
  "End"

example2 :: String
example2 =
  "Gain id1 { gain [ setValue 0.1, linearRampToValueAtTime 2 2.0 ] } [ output ] " <>
  "Oscillator id2 { type square frequency 440 } [ id1 ] " <>
  "End"
