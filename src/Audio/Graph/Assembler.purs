module Audio.Graph.Assembler (assemble) where

-- | Assemble an Audio Graph definition into a working assemblage of audio nodes

import Audio.Graph
import Audio.Buffer (AudioBuffers)
import Audio.Graph.Attributes (setOscillatorAttributes, setAudioBufferSourceAttributes,
    setGainAttributes, setDelayAttributes, setBiquadFilterAttributes,
    setStereoPannerAttributes)
import Audio.WebAudio.AudioContext (createBufferSource, createOscillator, createGain, createBiquadFilter,
    createDelay, createStereoPanner, destination, currentTime)
import Audio.WebAudio.Types (WebAudio, AudioContext, AudioNode(..),  connect, connectParam)
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
    StereoPannerType-> assembleStereoPanner ctx ass (NodeDef nd)

-- nodes

assembleOscillator :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleOscillator ctx ass (NodeDef nd) =
  trace ("assembling oscillator id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    oscNode <- createOscillator ctx
    _ <- setOscillatorAttributes now oscNode nd.attributes
    let
      ass' = insert nd.id (Oscillator oscNode) ass
    pure ass'

assembleGain :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleGain ctx ass (NodeDef nd) =
  trace ("assembling gain id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    gainNode <- createGain ctx
    _ <- setGainAttributes now gainNode nd.attributes
    let
      ass' = insert nd.id (Gain gainNode) ass
    pure ass'

assembleBiquadFilter :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleBiquadFilter ctx ass (NodeDef nd) =
  trace ("assembling biquad filter id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    biquadFilterNode <- createBiquadFilter ctx
    _ <- setBiquadFilterAttributes now biquadFilterNode nd.attributes
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
    now <- currentTime ctx
    delayNode <- createDelay ctx
    _ <- setDelayAttributes now delayNode nd.attributes
    let
      ass' = insert nd.id (Delay delayNode) ass
    pure ass'

assembleStereoPanner :: ∀ eff. AudioContext -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Assemblage)
assembleStereoPanner ctx ass (NodeDef nd) =
  trace ("assembling stereo panner id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    stereoPannerNode <- createStereoPanner ctx
    _ <- setStereoPannerAttributes now stereoPannerNode nd.attributes
    let
      ass' = insert nd.id (StereoPanner stereoPannerNode) ass
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
setConnections :: ∀ eff. AudioNode -> Assemblage -> Set Reference -> (Eff (wau :: WebAudio | eff) Unit)
setConnections sourceNode ass targets =
  traverse_ (setConnectionRef sourceNode ass) targets

setConnectionRef :: ∀ eff. AudioNode -> Assemblage -> Reference  -> (Eff (wau :: WebAudio | eff) Unit)
setConnectionRef sourceNode ass ref =
  case ref of
    NodeRef nodeId ->
      setConnection sourceNode ass nodeId
    ParameterRef nodeId parameterId ->
      setConnectionParam sourceNode ass nodeId parameterId

-- set one connection from a node to a target node
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
        StereoPanner n ->
          connect n targetNode
        Destination n ->
          connect n targetNode
    _ ->
      pure unit

-- set a connection from a node to an audio paramter on a target node
setConnectionParam :: ∀ eff. AudioNode -> Assemblage -> String  -> String -> (Eff (wau :: WebAudio | eff) Unit)
setConnectionParam sourceNode ass targetNode param =
  -- not yet implemented
  -- unsafeConnectParam modGainNode carrier "frequency"
  trace ("connecting to target: " <> targetNode <> "." <> param) \_ ->
  case lookup targetNode ass of
    Just targetNode ->
      -- this is very verbose but I haven't yet found a mechanism of generalising it
      case sourceNode of
        Gain n ->
          connectParam n targetNode param
        Oscillator n ->
          connectParam n targetNode param
        AudioBufferSource n ->
          connectParam n targetNode param
        BiquadFilter n ->
          connectParam n targetNode param
        Delay n ->
          connectParam n targetNode param
        Analyser n ->
          connectParam n targetNode param
        StereoPanner n ->
          connectParam n targetNode param
        Destination n ->
          connectParam n targetNode param
    _ ->
      pure unit
