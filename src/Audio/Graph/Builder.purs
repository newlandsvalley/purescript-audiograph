module Audio.Graph.Builder (build) where

-- | build an assemblage from an audio graph

import Control.Monad.Aff (Aff, liftEff')
import Network.HTTP.Affjax (AJAX)
import Data.Either (Either(..), either, isLeft)
import Data.Map (empty)
import Audio.Graph.ResourceLoader (loadBuffers)
import Audio.Graph.Assembler (assemble)
import Audio.Graph (AudioGraph, Assemblage)
import Audio.WebAudio.Types (WebAudio, AudioContext)
import Prelude (($), id, bind, pure)

-- | build an assemblage from an audio graph
build :: ∀ eff.
  AudioContext
  -> AudioGraph
  -> Aff (ajax :: AJAX, wau :: WebAudio | eff) (Either String Assemblage)
build ctx graph =
  do
    -- attempt to load any sound buffers from the specified URLs
    ebuffs <- loadBuffers ctx graph
    let
      buffers = either (\_ -> empty) id ebuffs
      loadError = either id (\_ -> "") ebuffs
    -- if the load fails, then we fail, otherwise build the assemblage
    if (isLeft ebuffs)
      then
        pure $ Left loadError
      else do
        assemblage <- liftEff' $ assemble ctx buffers graph
        pure $ Right assemblage
