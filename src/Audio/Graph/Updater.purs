module Audio.Graph.Updater (update) where

-- | Assemble an Audio Graph definition into a working assemblage of audio nodes

import Audio.Graph
import Audio.Buffer (AudioBuffers)
import Audio.Graph.Attributes (setOscillatorAttributes, setAudioBufferSourceAttributes,
  setGainAttributes, setDelayAttributes, setBiquadFilterAttributes)
import Audio.WebAudio.Types (WebAudio, AudioNode(..))
import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Data.Map (lookup, size)
import Data.Maybe (Maybe(..))
import Prelude (Unit, pure, show, unit, (<>), ($))

import Debug.Trace (trace)

-- | assemble the web-audio graph as a playable assemblage
update :: ∀ eff. AudioBuffers -> Assemblage -> AudioGraph -> (Eff (wau :: WebAudio | eff) Unit)
update buffers ass graph =
  trace "updating graph" \_ ->
  traverse_ (updateNode buffers ass) graph

updateNode :: ∀ eff. AudioBuffers -> Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Unit)
updateNode buffers ass (NodeDef nd) =
  trace ("update graph size: " <> (show $ size ass)) \_ ->
  case nd.nodeType of
    OscillatorType -> updateOscillator ass (NodeDef nd)
    AudioBufferSourceType -> updateAudioBufferSource ass buffers (NodeDef nd)
    GainType-> updateGain ass (NodeDef nd)
    BiquadFilterType-> updateBiquadFilter ass (NodeDef nd)
    DelayType-> updateDelay ass (NodeDef nd)

-- nodes

updateOscillator :: ∀ eff. Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Unit)
updateOscillator ass (NodeDef nd) =
  trace ("updating oscillator id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Oscillator node) ->
        setOscillatorAttributes node nd.attributes
      _ ->
        pure unit

updateGain :: ∀ eff. Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Unit)
updateGain ass (NodeDef nd) =
  trace ("updating gain id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Gain node) ->
        setGainAttributes node nd.attributes
      _ ->
        pure unit

updateBiquadFilter :: ∀ eff. Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Unit)
updateBiquadFilter ass (NodeDef nd) =
  trace ("updating biquad filter id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (BiquadFilter node) ->
        setBiquadFilterAttributes node nd.attributes
      _ ->
        pure unit

updateAudioBufferSource:: ∀ eff. Assemblage -> AudioBuffers -> NodeDef-> (Eff (wau :: WebAudio | eff) Unit)
updateAudioBufferSource ass buffers (NodeDef nd) =
  trace ("updating audio buffer source id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (AudioBufferSource node) ->
        setAudioBufferSourceAttributes node nd.attributes buffers
      _ ->
        pure unit

updateDelay :: ∀ eff. Assemblage -> NodeDef-> (Eff (wau :: WebAudio | eff) Unit)
updateDelay ass (NodeDef nd) =
  trace ("updating delay id: " <> nd.id) \_ ->
  let
    mNode = lookup nd.id ass
  in
    case mNode of
      Just (Delay node) ->
        setDelayAttributes node nd.attributes
      _ ->
        pure unit
