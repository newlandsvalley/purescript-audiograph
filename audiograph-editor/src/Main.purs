module Main where

import Prelude
import Audio.WebAudio.Types (WebAudio, AudioContext)
import Audio.WebAudio.AudioContext (makeAudioContext)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H

import JS.FileIO (FILEIO)
import Network.HTTP.Affjax (AJAX)

import Container as Container

main :: Eff (HA.HalogenEffects (ajax :: AJAX, wau :: WebAudio, fileio :: FILEIO )) Unit
main = HA.runHalogenAff do
  ctx <- H.liftEff makeAudioContext
  body <- HA.awaitBody
  runUI (Container.component ctx) unit body
