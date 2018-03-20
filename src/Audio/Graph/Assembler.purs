module Audio.Graph.Assembler (assemble) where

import Audio.Graph
import Audio.Graph.Attributes (AttributeMap,
  setOscillatorTypeAttr, setFrequencyAttr)

import Audio.WebAudio.AudioContext (createBufferSource, createOscillator, createGain, decodeAudioData, destination, makeAudioContext)
import Audio.WebAudio.Types (WebAudio, AudioContext, AudioNode(..), OscillatorNode, GainNode, connect)
import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_, foldM)
import Data.Map (insert, lookup, singleton)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Prelude (Unit, bind, pure, unit)

assemble :: ∀ eff. AudioGraph -> (Eff (wau :: WebAudio | eff) Assemblage)
assemble graph = do
  ctx <- makeAudioContext
  destNode <- destination ctx
  -- the assembled nodes always contain a destination node called 'output'
  let
    ass = singleton "output" (Destination destNode)
    --  foldM :: forall f m a b. Foldable f => Monad m => (a -> b -> m a) -> a -> f b -> m a
  ass' <- foldM (assembleNode ctx) ass graph
  pure ass'

assembleNode :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleNode ctx ass (NodeDef nd) =
  case nd.nodeType of
    OscillatorType -> assembleOscillator ctx ass (NodeDef nd)
    GainType-> assembleGain ctx ass (NodeDef nd)

-- nodes

assembleOscillator :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleOscillator ctx ass (NodeDef nd) = do
  oscNode <- createOscillator ctx
  _ <- setConnections (Oscillator oscNode) ass nd.connections
  _ <- setOscillatorAttributes oscNode nd.attributes
  let
    ass' = insert nd.id (Oscillator oscNode) ass
  pure ass'

assembleGain :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleGain ctx ass (NodeDef nd) = do
  gainNode <- createGain ctx
  _ <- setConnections (Gain gainNode) ass nd.connections
  let
    ass' = insert nd.id (Gain gainNode) ass
  pure ass'

-- connections

setConnections :: ∀ eff. AudioNode -> Assemblage -> Set String -> (Eff (wau :: WebAudio | eff) Unit)
setConnections sourceNode ass targets =
  traverse_ (setConnection sourceNode ass) targets

setConnection :: ∀ eff. AudioNode -> Assemblage -> String  -> (Eff (wau :: WebAudio | eff) Unit)
setConnection sourceNode ass target =
  case lookup target ass of
    Just targetNode ->
      -- this is very verbose but I haven't yet found a mechanism of generalising it
      case sourceNode of
        Gain n ->
          connect n targetNode
        Oscillator n ->
          connect n targetNode
        AudioBufferSource n ->
          connect n targetNode
        BiquadFilter n ->
          connect n targetNode
        Delay n ->
          connect n targetNode
        Analyser n ->
          connect n targetNode
        Destination n ->
          connect n targetNode
    _ ->
      pure unit

-- attributes

setOscillatorAttributes :: ∀ eff. OscillatorNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setOscillatorAttributes osc map = do
  _ <- setOscillatorTypeAttr osc map
  setFrequencyAttr osc map
