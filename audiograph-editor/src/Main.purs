module Main where

import Prelude
import Audio.WebAudio.Types (WebAudio, AudioContext)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import JS.FileIO (FILEIO)
import Network.HTTP.Affjax (AJAX)

import Container as Container

main :: Eff (HA.HalogenEffects (ajax :: AJAX, wau :: WebAudio, fileio :: FILEIO )) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Container.component unit body
