module Audio.Graph.Assembler (assemble) where

-- | Assemble an Audio Graph definition into a working assemblage of audio nodes

import Audio.Graph
import Audio.Buffer (AudioBuffers)
import Audio.Graph.Attributes (setOscillatorAttributes, setAudioBufferSourceAttributes,
    setGainAttributes, setDelayAttributes, setBiquadFilterAttributes,
    setPannerAttributes, setStereoPannerAttributes, setDynamicsCompressorAttributes,
    setConvolverAttributes)
import Audio.WebAudio.BaseAudioContext (createBufferSource, createOscillator,
    createGain, createBiquadFilter, createConvolver, createPanner,
    createDelay, createStereoPanner, createDynamicsCompressor, destination, currentTime)
import Audio.WebAudio.Types (AudioContext, AudioNode(..),  connect, connectParam)
import Effect (Effect)
import Data.Foldable (traverse_, foldM)
import Data.Map (insert, lookup, singleton)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Prelude (Unit, bind, pure, unit)

-- | assemble the web-audio graph as a playable assemblage
assemble :: AudioContext -> AudioBuffers -> AudioGraph -> Effect Assemblage
assemble ctx buffers graph =
  -- trace "assembling graph" \_ ->
  do
    destNode <- destination ctx
    -- the assembled nodes always contain a destination node called 'output'
    let
      node0 = singleton "output" (Destination destNode)
      listener = Nothing
      --  foldM :: forall f m a b. Foldable f => Monad m => (a -> b -> m a) -> a -> f b -> m a
    nodes <- foldM (assembleNode ctx buffers) node0 graph.nodeDefs
    -- assemble the connections after the node assemblage has been built
    _ <- traverse_ (assembleConnections nodes ) graph.nodeDefs
    pure { nodes, listener }

assembleNode :: AudioContext -> AudioBuffers -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assembleNode ctx buffers ass (NodeDef nd) =
  -- trace ("assemblage size: " <> (show $ size ass)) \_ ->
  case nd.nodeType of
    OscillatorType -> assembleOscillator ctx ass (NodeDef nd)
    AudioBufferSourceType -> assembleAudioBufferSource ctx ass buffers (NodeDef nd)
    GainType-> assembleGain ctx ass (NodeDef nd)
    BiquadFilterType-> assembleBiquadFilter ctx ass (NodeDef nd)
    DelayType-> assembleDelay ctx ass (NodeDef nd)
    StereoPannerType-> assembleStereoPanner ctx ass (NodeDef nd)
    DynamicsCompressorType-> assembleDynamicsCompressor ctx ass (NodeDef nd)
    ConvolverType -> assembleConvolver ctx ass buffers (NodeDef nd)
    PannerType -> assemblePanner ctx ass (NodeDef nd)


-- nodes

assembleOscillator :: AudioContext -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assembleOscillator ctx ass (NodeDef nd) =
  -- trace ("assembling oscillator id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    oscNode <- createOscillator ctx
    _ <- setOscillatorAttributes now oscNode nd.attributes
    let
      ass' = insert nd.id (Oscillator oscNode) ass
    pure ass'

assembleGain :: AudioContext -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assembleGain ctx ass (NodeDef nd) =
  -- trace ("assembling gain id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    gainNode <- createGain ctx
    _ <- setGainAttributes now gainNode nd.attributes
    let
      ass' = insert nd.id (Gain gainNode) ass
    pure ass'

assembleBiquadFilter :: AudioContext -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assembleBiquadFilter ctx ass (NodeDef nd) =
  -- trace ("assembling biquad filter id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    biquadFilterNode <- createBiquadFilter ctx
    _ <- setBiquadFilterAttributes now biquadFilterNode nd.attributes
    let
      ass' = insert nd.id (BiquadFilter biquadFilterNode) ass
    pure ass'

assembleAudioBufferSource :: AudioContext -> AssembledNodes -> AudioBuffers -> NodeDef-> Effect AssembledNodes
assembleAudioBufferSource ctx ass buffers (NodeDef nd) =
  -- trace ("assembling audio buffer source id: " <> nd.id) \_ ->
  do
    audioBufferSourceNode <- createBufferSource ctx
    _ <- setAudioBufferSourceAttributes audioBufferSourceNode nd.attributes buffers
    let
      ass' = insert nd.id (AudioBufferSource audioBufferSourceNode) ass
    pure ass'

assembleDelay :: AudioContext -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assembleDelay ctx ass (NodeDef nd) =
  -- trace ("assembling delay id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    delayNode <- createDelay ctx
    _ <- setDelayAttributes now delayNode nd.attributes
    let
      ass' = insert nd.id (Delay delayNode) ass
    pure ass'

assembleStereoPanner :: AudioContext -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assembleStereoPanner ctx ass (NodeDef nd) =
  -- trace ("assembling stereo panner id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    stereoPannerNode <- createStereoPanner ctx
    _ <- setStereoPannerAttributes now stereoPannerNode nd.attributes
    let
      ass' = insert nd.id (StereoPanner stereoPannerNode) ass
    pure ass'

assemblePanner :: AudioContext -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assemblePanner ctx ass (NodeDef nd) =
  -- trace ("assembling stereo panner id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    pannerNode <- createPanner ctx
    _ <- setPannerAttributes now pannerNode nd.attributes
    let
      ass' = insert nd.id (Panner pannerNode) ass
    pure ass'

assembleDynamicsCompressor :: AudioContext -> AssembledNodes -> NodeDef-> Effect AssembledNodes
assembleDynamicsCompressor ctx ass (NodeDef nd) =
  -- trace ("assembling dynamics compressor id: " <> nd.id) \_ ->
  do
    now <- currentTime ctx
    dynamicsCompressorNode <- createDynamicsCompressor ctx
    _ <- setDynamicsCompressorAttributes now dynamicsCompressorNode nd.attributes
    let
      ass' = insert nd.id (DynamicsCompressor dynamicsCompressorNode) ass
    pure ass'

assembleConvolver :: AudioContext -> AssembledNodes -> AudioBuffers -> NodeDef-> Effect AssembledNodes
assembleConvolver ctx ass buffers (NodeDef nd) =
  -- trace ("assembling convolver id: " <> nd.id) \_ ->
  do
    convolverNode <- createConvolver ctx
    _ <- setConvolverAttributes convolverNode nd.attributes buffers
    let
      ass' = insert nd.id (Convolver convolverNode) ass
    pure ass'

-- connections

-- assemble connections from the node defined in the NodeDef
assembleConnections :: AssembledNodes -> NodeDef-> Effect Unit
assembleConnections ass (NodeDef nd) =
  -- trace ("assembling connections for node: " <> nd.id) \_ ->
  let
    maybeAudioNode = lookup nd.id ass
  in
    case maybeAudioNode of
      Just node ->
        setConnections node ass nd.connections
      Nothing -> -- can't happen - maybe use Partial
        pure unit

-- set all the connections from one node
setConnections :: AudioNode -> AssembledNodes -> Set Reference -> Effect Unit
setConnections sourceNode ass targets =
  traverse_ (setConnectionRef sourceNode ass) targets

setConnectionRef :: AudioNode -> AssembledNodes -> Reference  -> Effect Unit
setConnectionRef sourceNode ass ref =
  case ref of
    NodeRef nodeId ->
      setConnection sourceNode ass nodeId
    ParameterRef nodeId parameterId ->
      setConnectionParam sourceNode ass nodeId parameterId

-- set one connection from a node to a target node
setConnection :: AudioNode -> AssembledNodes -> String  -> Effect Unit
setConnection sourceNode ass target =
  -- trace ("connecting to target: " <> target) \_ ->
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
        DynamicsCompressor n ->
          connect n targetNode
        Convolver n ->
          connect n targetNode
        Destination n ->
          connect n targetNode
        Panner n ->
          connect n targetNode
    _ ->
      pure unit

-- set a connection from a node to an audio paramter on a target node
setConnectionParam :: AudioNode -> AssembledNodes -> String  -> String -> Effect Unit
setConnectionParam sourceNode ass target param =
  -- not yet implemented
  -- unsafeConnectParam modGainNode carrier "frequency"
  -- trace ("connecting to target: " <> targetNode <> "." <> param) \_ ->
  case lookup target ass of
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
        DynamicsCompressor n ->
          connectParam n targetNode param
        Convolver n ->
          connectParam n targetNode param
        Destination n ->
          connectParam n targetNode param
        Panner n ->
          connectParam n targetNode param
    _ ->
      pure unit
