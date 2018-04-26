module Main where

import Prelude
import Audio.WebAudio.Types (AUDIO)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H

import JS.FileIO (FILEIO)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Random (RANDOM)
import Container as Container

main :: Eff (HA.HalogenEffects (ajax :: AJAX, audio :: AUDIO, fileio :: FILEIO, random :: RANDOM )) Unit
main = HA.runHalogenAff do
  ctx <- H.liftEff newAudioContext
  body <- HA.awaitBody
  runUI (Container.component ctx) unit body
