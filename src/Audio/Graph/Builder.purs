module Audio.Graph.Builder (build) where

-- | build an assemblage from an audio graph

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Data.Either (Either(..), either, isLeft)
import Data.Map (empty)
import Audio.Graph.ResourceLoader (loadBuffers)
import Audio.Graph.Assembler (assemble)
import Audio.Graph (AudioGraph, Assemblage)
import Audio.WebAudio.Types (AudioContext)
import Prelude (($), identity, bind, pure)

-- | build an assemblage from an audio graph
build ::
  AudioContext
  -> AudioGraph
  -> Aff (Either String Assemblage)
build ctx graph =
  do
    -- attempt to load any sound buffers from the specified URLs
    ebuffs <- loadBuffers ctx graph
    let
      buffers = either (\_ -> empty) identity ebuffs
      loadError = either identity (\_ -> "") ebuffs
    -- if the load fails, then we fail, otherwise build the assemblage
    if (isLeft ebuffs)
      then
        pure $ Left loadError
      else do
        assemblage <- liftEffect $ assemble ctx buffers graph
        pure $ Right assemblage
