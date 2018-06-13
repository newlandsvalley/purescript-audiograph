module Main where

import Prelude
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H

import Container as Container

main :: Effect Unit
main = HA.runHalogenAff do
  ctx <- H.liftEffect newAudioContext
  body <- HA.awaitBody
  runUI (Container.component ctx) unit body
