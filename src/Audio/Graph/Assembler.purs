module Audio.Graph.Assembler (assemble) where

-- | Assemble an Audio Graph definition into a working assemblage of audio nodes

import Audio.Graph
import Audio.Buffer (AudioBuffers)
import Audio.Graph.Attributes (AttributeMap,
  setOscillatorTypeAttr, setOscillatorFrequencyAttr, setGainAttr, setBiquadFilterTypeAttr,
  setDelayAttr, setBiquadFilterFrequencyAttr, setAudioBufferAttr)
import Audio.WebAudio.AudioContext (createBufferSource, createOscillator, createGain, createBiquadFilter,
    createDelay, destination)
import Audio.WebAudio.Types (WebAudio, AudioContext, AudioNode(..), OscillatorNode, GainNode,
  BiquadFilterNode, AudioBufferSourceNode, DelayNode, connect)
import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_, foldM)
import Data.Map (insert, lookup, singleton, size)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Prelude (Unit, bind, pure, show, unit, (<>), ($))

import Debug.Trace (trace)

-- | assemble the web-audio graph as a playable assemblage
assemble :: ∀ eff. AudioContext -> AudioBuffers -> AudioGraph -> (Eff (wau :: WebAudio | eff) Assemblage)
assemble ctx buffers graph =
  trace "assembling graph" \_ ->
  do
    destNode <- destination ctx
    -- the assembled nodes always contain a destination node called 'output'
    let
      ass = singleton "output" (Destination destNode)
      --  foldM :: forall f m a b. Foldable f => Monad m => (a -> b -> m a) -> a -> f b -> m a
    ass' <- foldM (assembleNode ctx buffers) ass graph
    -- assemble the connections after the node assemblage has been built
    _ <- traverse_ (assembleConnections ass') graph
    pure ass'

assembleNode :: ∀ eff. AudioContext -> AudioBuffers -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleNode ctx buffers ass (NodeDef nd) =
  trace ("assemblage size: " <> (show $ size ass)) \_ ->
  case nd.nodeType of
    OscillatorType -> assembleOscillator ctx ass (NodeDef nd)
    AudioBufferSourceType -> assembleAudioBufferSource ctx ass buffers (NodeDef nd)
    GainType-> assembleGain ctx ass (NodeDef nd)
    BiquadFilterType-> assembleBiquadFilter ctx ass (NodeDef nd)
    DelayType-> assembleDelay ctx ass (NodeDef nd)

-- nodes

assembleOscillator :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleOscillator ctx ass (NodeDef nd) =
  trace ("assembling oscillator id: " <> nd.id) \_ ->
  do
    oscNode <- createOscillator ctx
    _ <- setOscillatorAttributes oscNode nd.attributes
    let
      ass' = insert nd.id (Oscillator oscNode) ass
    pure ass'

assembleGain :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleGain ctx ass (NodeDef nd) =
  trace ("assembling gain id: " <> nd.id) \_ ->
  do
    gainNode <- createGain ctx
    _ <- setGainAttributes gainNode nd.attributes
    let
      ass' = insert nd.id (Gain gainNode) ass
    pure ass'

assembleBiquadFilter :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleBiquadFilter ctx ass (NodeDef nd) =
  trace ("assembling biquad filter id: " <> nd.id) \_ ->
  do
    biquadFilterNode <- createBiquadFilter ctx
    _ <- setBiquadFilterAttributes biquadFilterNode nd.attributes
    let
      ass' = insert nd.id (BiquadFilter biquadFilterNode) ass
    pure ass'

assembleAudioBufferSource :: ∀ eff. AudioContext -> Assemblage -> AudioBuffers -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleAudioBufferSource ctx ass buffers (NodeDef nd) =
  trace ("assembling audio buffer source id: " <> nd.id) \_ ->
  do
    audioBufferSourceNode <- createBufferSource ctx
    _ <- setAudioBufferSourceAttributes audioBufferSourceNode nd.attributes buffers
    let
      ass' = insert nd.id (AudioBufferSource audioBufferSourceNode) ass
    pure ass'

assembleDelay :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleDelay ctx ass (NodeDef nd) =
  trace ("assembling delay id: " <> nd.id) \_ ->
  do
    delayNode <- createDelay ctx
    -- _ <- setDelayAttributes delayNode nd.attributes
    let
      ass' = insert nd.id (Delay delayNode) ass
    pure ass'
-- connections

-- assemble connections from the node defined in the NodeDef
assembleConnections :: ∀ eff. Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Unit)
assembleConnections ass (NodeDef nd) =
  trace ("assembling connections for node: " <> nd.id) \_ ->
  let
    maybeAudioNode = lookup nd.id ass
  in
    case maybeAudioNode of
      Just node ->
        setConnections node ass nd.connections
      Nothing -> -- can't happen - maybe use Partial
        pure unit

-- set all the connections from one node
setConnections :: ∀ eff. AudioNode -> Assemblage -> Set String -> (Eff (wau :: WebAudio | eff) Unit)
setConnections sourceNode ass targets =
  traverse_ (setConnection sourceNode ass) targets

-- set one connection from a node
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

setAudioBufferSourceAttributes :: ∀ eff. AudioBufferSourceNode -> AttributeMap -> AudioBuffers -> (Eff (wau :: WebAudio | eff) Unit)
setAudioBufferSourceAttributes node map buffers = do
  setAudioBufferAttr node map buffers

setGainAttributes :: ∀ eff. GainNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setGainAttributes gain map = do
  setGainAttr gain map

setDelayAttributes :: ∀ eff. DelayNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setDelayAttributes delayNode map = do
  setDelayAttr delayNode map

setBiquadFilterAttributes :: ∀ eff. BiquadFilterNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setBiquadFilterAttributes bqf map = do
  _ <- setBiquadFilterTypeAttr bqf map
  setBiquadFilterFrequencyAttr bqf map
