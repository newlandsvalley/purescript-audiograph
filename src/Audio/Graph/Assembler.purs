module Audio.Graph.Assembler (assemble) where

-- | Assemble an Audio Graph definition into a working assemblage of audio nodes

import Audio.Graph
import Audio.Graph.Attributes (AttributeMap,
  setOscillatorTypeAttr, setOscillatorFrequencyAttr, setGainAttr, setBiquadFilterTypeAttr,
  setBiquadFilterFrequencyAttr)

import Audio.WebAudio.AudioContext (createBufferSource, createOscillator, createGain, createBiquadFilter,
    decodeAudioData, destination)
import Audio.WebAudio.Types (WebAudio, AudioContext, AudioNode(..), OscillatorNode, GainNode,
  BiquadFilterNode, connect)
import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_, foldM)
import Data.Map (insert, lookup, singleton, size)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Prelude (Unit, bind, pure, show, unit, (<>), ($))

import Debug.Trace (trace)

assemble :: ∀ eff. AudioContext -> AudioGraph -> (Eff (wau :: WebAudio | eff) Assemblage)
assemble ctx graph =
  trace "assembling graph" \_ ->
  do
    destNode <- destination ctx
    -- the assembled nodes always contain a destination node called 'output'
    let
      ass = singleton "output" (Destination destNode)
      --  foldM :: forall f m a b. Foldable f => Monad m => (a -> b -> m a) -> a -> f b -> m a
    ass' <- foldM (assembleNode ctx) ass graph
    pure ass'

assembleNode :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleNode ctx ass (NodeDef nd) =
  trace ("assemblage size: " <> (show $ size ass)) \_ ->
  case nd.nodeType of
    OscillatorType -> assembleOscillator ctx ass (NodeDef nd)
    AudioBufferSourceType -> assembleAudioBufferSource ctx ass (NodeDef nd)
    GainType-> assembleGain ctx ass (NodeDef nd)
    BiquadFilterType-> assembleBiquadFilter ctx ass (NodeDef nd)

-- nodes

assembleOscillator :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleOscillator ctx ass (NodeDef nd) =
  trace ("assembling oscillator id: " <> nd.id) \_ ->
  do
    oscNode <- createOscillator ctx
    _ <- setConnections (Oscillator oscNode) ass nd.connections
    _ <- setOscillatorAttributes oscNode nd.attributes
    let
      ass' = insert nd.id (Oscillator oscNode) ass
    pure ass'

assembleGain :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleGain ctx ass (NodeDef nd) =
  trace ("assembling gain id: " <> nd.id) \_ ->
  do
    gainNode <- createGain ctx
    _ <- setConnections (Gain gainNode) ass nd.connections
    _ <- setGainAttributes gainNode nd.attributes
    let
      ass' = insert nd.id (Gain gainNode) ass
    pure ass'

assembleBiquadFilter :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleBiquadFilter ctx ass (NodeDef nd) =
  trace ("assembling biquad filter id: " <> nd.id) \_ ->
  do
    biquadFilterNode <- createBiquadFilter ctx
    _ <- setConnections (BiquadFilter biquadFilterNode) ass nd.connections
    _ <- setBiquadFilterAttributes biquadFilterNode nd.attributes
    let
      ass' = insert nd.id (BiquadFilter biquadFilterNode) ass
    pure ass'

assembleAudioBufferSource :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleAudioBufferSource ctx ass (NodeDef nd) =
  trace ("assembling audio buffer source id: " <> nd.id) \_ ->
  do
    pure ass


-- connections

setConnections :: ∀ eff. AudioNode -> Assemblage -> Set String -> (Eff (wau :: WebAudio | eff) Unit)
setConnections sourceNode ass targets =
  traverse_ (setConnection sourceNode ass) targets

setConnection :: ∀ eff. AudioNode -> Assemblage -> String  -> (Eff (wau :: WebAudio | eff) Unit)
setConnection sourceNode ass target =
  trace ("connecting to target: " <> target) \_ ->
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
  setOscillatorFrequencyAttr osc map

setGainAttributes :: ∀ eff. GainNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setGainAttributes gain map = do
  setGainAttr gain map

setBiquadFilterAttributes :: ∀ eff. BiquadFilterNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setBiquadFilterAttributes bqf map = do
  _ <- setBiquadFilterTypeAttr bqf map
  setBiquadFilterFrequencyAttr bqf map
