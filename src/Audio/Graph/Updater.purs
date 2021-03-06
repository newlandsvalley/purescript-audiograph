module Audio.Graph.Updater (update) where

-- | Assemble an Audio Graph update definition into a working assemblage of audio node deltas

import Audio.Graph
import Audio.Buffer (AudioBuffers)
import Audio.WebAudio.BaseAudioContext (currentTime)
import Audio.Graph.Attributes (setOscillatorAttributes, setAudioBufferSourceAttributes,
  setGainAttributes, setDelayAttributes, setBiquadFilterAttributes,
  setPannerAttributes, setStereoPannerAttributes, setDynamicsCompressorAttributes,
  setConvolverAttributes)
import Audio.WebAudio.Types (AudioContext, AudioNode(..))
import Effect (Effect)
import Data.Foldable (traverse_)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Prelude (Unit, bind, pure, unit)


-- | assemble the update web-audio graph as a playable assemblage
update :: AudioContext -> AudioBuffers -> Assemblage -> AudioGraph -> Effect Unit
update ctx buffers ass graph =
  -- trace "updating graph" \_ ->
  do
    now <- currentTime ctx
    traverse_ (updateNode now buffers ass.nodes) graph.nodeDefs

updateNode :: Number -> AudioBuffers -> AssembledNodes -> NodeDef-> Effect Unit
updateNode startTime buffers ass (NodeDef nd) =
  -- trace ("update graph size: " <> (show $ size ass)) \_ ->
  case nd.nodeType of
    OscillatorType -> updateOscillator startTime ass (NodeDef nd)
    AudioBufferSourceType -> updateAudioBufferSource ass buffers (NodeDef nd)
    GainType-> updateGain startTime ass (NodeDef nd)
    BiquadFilterType-> updateBiquadFilter startTime ass (NodeDef nd)
    DelayType-> updateDelay startTime ass (NodeDef nd)
    StereoPannerType-> updateStereoPanner startTime ass (NodeDef nd)
    DynamicsCompressorType -> updateDynamicsCompressor startTime ass (NodeDef nd)
    ConvolverType -> updateConvolver ass buffers (NodeDef nd)
    PannerType -> updatePanner startTime ass (NodeDef nd)

-- nodes

updateOscillator :: Number -> AssembledNodes -> NodeDef-> Effect Unit
updateOscillator startTime ass (NodeDef nd) =
  -- trace ("updating oscillator id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Oscillator node) ->
        setOscillatorAttributes startTime node nd.attributes
      _ ->
        pure unit

updateGain :: Number -> AssembledNodes -> NodeDef-> Effect Unit
updateGain startTime ass (NodeDef nd) =
  -- trace ("updating gain id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Gain node) ->
        setGainAttributes startTime node nd.attributes
      _ ->
        pure unit

updateBiquadFilter :: Number -> AssembledNodes -> NodeDef-> Effect Unit
updateBiquadFilter startTime ass (NodeDef nd) =
  -- trace ("updating biquad filter id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (BiquadFilter node) ->
        setBiquadFilterAttributes startTime node nd.attributes
      _ ->
        pure unit

updateAudioBufferSource:: AssembledNodes -> AudioBuffers -> NodeDef-> Effect Unit
updateAudioBufferSource ass buffers (NodeDef nd) =
  -- trace ("updating audio buffer source id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (AudioBufferSource node) ->
        setAudioBufferSourceAttributes node nd.attributes buffers
      _ ->
        pure unit

updateDelay :: Number -> AssembledNodes -> NodeDef-> Effect Unit
updateDelay startTime ass (NodeDef nd) =
  -- trace ("updating delay id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Delay node) ->
        setDelayAttributes startTime node nd.attributes
      _ ->
        pure unit

updateStereoPanner :: Number -> AssembledNodes -> NodeDef-> Effect Unit
updateStereoPanner startTime ass (NodeDef nd) =
  -- trace ("updating stereo panner id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (StereoPanner node) ->
        setStereoPannerAttributes startTime node nd.attributes
      _ ->
        pure unit

updatePanner :: Number -> AssembledNodes -> NodeDef-> Effect Unit
updatePanner startTime ass (NodeDef nd) =
  -- trace ("updating stereo panner id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Panner node) ->
        setPannerAttributes startTime node nd.attributes
      _ ->
        pure unit

updateDynamicsCompressor:: Number -> AssembledNodes -> NodeDef-> Effect Unit
updateDynamicsCompressor startTime ass (NodeDef nd) =
  -- trace ("updating dynamics compressor id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (DynamicsCompressor node) ->
        setDynamicsCompressorAttributes startTime node nd.attributes
      _ ->
        pure unit

updateConvolver:: AssembledNodes -> AudioBuffers -> NodeDef-> Effect Unit
updateConvolver ass buffers (NodeDef nd) =
  -- trace ("updating convolver id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Convolver node) ->
        setConvolverAttributes node nd.attributes buffers
      _ ->
        pure unit
