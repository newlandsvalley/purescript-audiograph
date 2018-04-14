module Audio.Graph.Attributes
  (AudioAttribute, AttributeMap, AudioParamDef(..), Time(..),
   addOscillatorType, addFrequency,
   getOscillatorType, getNumber, getString, biquadFilterTypeAttr,
   oscillatorTypeAttr, numberAttr, stringAttr, boolAttr, audioParamsAttr,
   setOscillatorAttributes, setAudioBufferSourceAttributes,
   setGainAttributes, setDelayAttributes, setBiquadFilterAttributes,
   setStereoPannerAttributes, setDynamicsCompressorAttributes,
   setConvolverAttributes
   ) where

-- | Audio node attributes.  These are either simple or consist of
-- | AudioParams (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam)
-- | which have special properties

import Prelude (Unit, ($), (<$>), (<$), (#), (>>=), (<>), (+), id, bind, pure, unit)
import Control.Monad.Eff (Eff)
import Audio.WebAudio.Types (WebAudio, OscillatorNode, GainNode, BiquadFilterNode,
  AudioBufferSourceNode, DelayNode, StereoPannerNode, ConvolverNode,
  DynamicsCompressorNode, AudioParam, AudioBuffer)
import Audio.WebAudio.Oscillator (OscillatorType, detune, frequency, setOscillatorType)
import Audio.WebAudio.BiquadFilterNode (BiquadFilterType, setFilterType,
   filterFrequency, quality)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.AudioParam (setValue, setValueAtTime,
  linearRampToValueAtTime, exponentialRampToValueAtTime)
import Audio.WebAudio.AudioBufferSourceNode (setBuffer, setLoop, setLoopStart, setLoopEnd)
import Audio.WebAudio.DelayNode (delayTime)
import Audio.WebAudio.StereoPannerNode (pan)
import Audio.WebAudio.DynamicsCompressorNode (threshold, knee, ratio, attack, release)
import Audio.WebAudio.ConvolverNode (setBuffer, normalize) as Convolver
import Audio.Buffer (AudioBuffers)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..), length)
import Data.Symbol (SProxy(..))
import Data.Foldable (traverse_)
import Data.Variant (Variant, inj, on, default)

import Debug.Trace (trace, traceShow)

-- | the (type of) an attribute of an audio node
type AudioAttribute = Variant ( oscillatorType :: OscillatorType
                              , biquadFilterType :: BiquadFilterType
                              , number :: Number
                              , string :: String
                              , bool :: Boolean
                              , audioParams :: List AudioParamDef)

-- | a map of a set of such (named) attributes
type AttributeMap = Map String AudioAttribute


data Time =
    Absolute Number
  | Relative Number

-- | an AudioParam definition
-- | see https://developer.mozilla.org/en-US/docs/Web/API/AudioParam
data AudioParamDef =
    SetValue Number
  | SetValueAtTime Number Time
  | LinearRampToValueAtTime Number Time
  | ExponentialRampToValueAtTime Number Time

-- data types
_oscillatorType = SProxy :: SProxy "oscillatorType"
_biquadFilterType = SProxy :: SProxy "biquadFilterType"
_number = SProxy :: SProxy "number"
_string = SProxy :: SProxy "string"
_bool = SProxy :: SProxy "bool"
_audioParams = SProxy :: SProxy "audioParams"

-- variant builders
oscillatorTypeAttr :: OscillatorType -> AudioAttribute
oscillatorTypeAttr t =
  inj _oscillatorType t

biquadFilterTypeAttr :: BiquadFilterType -> AudioAttribute
biquadFilterTypeAttr t =
  inj _biquadFilterType t

numberAttr :: Number -> AudioAttribute
numberAttr t =
  inj _number t

stringAttr :: String -> AudioAttribute
stringAttr t =
  inj _string t

boolAttr :: Boolean -> AudioAttribute
boolAttr t =
  inj _bool t

audioParamsAttr :: List AudioParamDef -> AudioAttribute
audioParamsAttr t =
  inj _audioParams t


-- add attributes to the map - we probbaly don't need this section

addOscillatorType :: AttributeMap -> OscillatorType -> AttributeMap
addOscillatorType map t =
  let
    attr = inj _oscillatorType t
  in
    insert "type" attr map

addFrequency :: AttributeMap -> Number -> AttributeMap
addFrequency map n =
  let
    attr = inj _number n
  in
    insert "frequency" attr map

-- get an attribute from a variant (if it's there)

getVNumber :: AudioAttribute -> Maybe Number
getVNumber =
    default Nothing
      # on _number Just

getVString :: AudioAttribute -> Maybe String
getVString =
    default Nothing
      # on _string Just

getVBool :: AudioAttribute -> Maybe Boolean
getVBool =
    default Nothing
      # on _bool Just

getVAudioParams :: AudioAttribute -> List AudioParamDef
getVAudioParams  =
    default Nil
      # on _audioParams id

-- get attributes from the map

getOscillatorType :: AttributeMap -> Maybe OscillatorType
getOscillatorType map =
  (lookup "type" map) >>= getOscillatorVal
    where
      getOscillatorVal :: AudioAttribute -> Maybe OscillatorType
      getOscillatorVal =
        default Nothing
          # on _oscillatorType Just

getBiquadFilterType :: AttributeMap -> Maybe BiquadFilterType
getBiquadFilterType map =
  (lookup "type" map) >>= getBiquadVal
    where
      getBiquadVal :: AudioAttribute -> Maybe BiquadFilterType
      getBiquadVal =
        default Nothing
          # on _biquadFilterType Just

-- | get a named Number attribute
getNumber :: String -> AttributeMap -> Maybe Number
getNumber attName map =
  (lookup attName map) >>= getVNumber

-- | get a named String attribute
getString :: String -> AttributeMap -> Maybe String
getString attName map =
  (lookup attName map) >>= getVString

-- | get a named Boolean attribute
getBool :: String -> AttributeMap -> Maybe Boolean
getBool attName map =
  (lookup attName map) >>= getVBool

-- | get a named audio Params attribute
getAudioParams :: String -> AttributeMap -> List AudioParamDef
getAudioParams attName map =
  let
    paramList =
      fromMaybe Nil $ getVAudioParams <$> (lookup attName map)
  in
    trace ("audio params for: " <> attName) \_ ->
    traceShow (length paramList) \_ ->
    paramList

-- set Audio attributes from values in the map

setOscillatorTypeAttr :: ∀ eff. OscillatorNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setOscillatorTypeAttr osc map =
  case getOscillatorType map of
    Just t ->
      setOscillatorType t osc
    _ ->
      pure unit

setBiquadFilterTypeAttr :: ∀ eff. BiquadFilterNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setBiquadFilterTypeAttr  bqf map =
  case getBiquadFilterType map of
    Just t ->
      setFilterType t bqf
    _ ->
      pure unit

-- | set the oscillator frequency
setOscillatorFrequencyAttr :: ∀ eff. Number -> OscillatorNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setOscillatorFrequencyAttr startTime osc map =
  case getAudioParams "frequency" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- frequency osc
        setParams startTime audioParam ps

-- | set the oscillator detune parameter
setOscillatorDetuneAttr :: ∀ eff. Number -> OscillatorNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setOscillatorDetuneAttr startTime osc map =
  case getAudioParams "detune" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- detune osc
        setParams startTime audioParam ps


-- | set the biquad filter frequency
setBiquadFilterFrequencyAttr :: ∀ eff. Number -> BiquadFilterNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setBiquadFilterFrequencyAttr startTime bqf map =
  case getAudioParams "frequency" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- filterFrequency bqf
        setParams startTime audioParam ps

-- | set the biquad filter quality
setBiquadFilterQualityAttr :: ∀ eff. Number -> BiquadFilterNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setBiquadFilterQualityAttr startTime bqf map =
  case getAudioParams "quality" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- quality bqf
        setParams startTime audioParam ps


setGainAttr :: ∀ eff. Number -> GainNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setGainAttr startTime gainNode map =
  case getAudioParams "gain" map of
    Nil ->
      pure unit
    ps ->
      do
        gainParam <- gain gainNode
        setParams startTime gainParam ps

setDelayAttr :: ∀ eff. Number -> DelayNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setDelayAttr startTime delayNode map =
  case getAudioParams "delayTime" map of
    Nil ->
      pure unit
    ps ->
      do
        delayParam <- delayTime delayNode
        setParams startTime delayParam ps

setStereoPannerAttr :: ∀ eff. Number -> StereoPannerNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setStereoPannerAttr startTime stereoPannerNode map =
  case getAudioParams "pan" map of
    Nil ->
      pure unit
    ps ->
      do
        panParam <- pan stereoPannerNode
        setParams startTime panParam ps


setCompressorThresholdAttr :: ∀ eff. Number -> DynamicsCompressorNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setCompressorThresholdAttr startTime compressorNode map =
  case getAudioParams "threshold" map of
    Nil ->
      pure unit
    ps ->
      do
        thresholdParam <- threshold compressorNode
        setParams startTime thresholdParam ps

-- generalised function  to set the value of a named audio parameter on a
-- dynamics compressor node
setCompressorParam :: ∀ eff.
    Number
    -> DynamicsCompressorNode
    -> String
    -> (DynamicsCompressorNode -> (Eff (wau :: WebAudio | eff) AudioParam))
    -> AttributeMap
    -> Eff ( wau :: WebAudio | eff) Unit
setCompressorParam startTime compressorNode attrName getf map =
  case getAudioParams attrName map of
    Nil ->
      pure unit
    ps ->
      do
        param <- getf compressorNode
        setParams startTime param ps


setAudioBufferAttr :: ∀ eff. AudioBufferSourceNode -> AttributeMap -> AudioBuffers -> Eff ( wau :: WebAudio | eff) Unit
setAudioBufferAttr audioBufferNode attMap buffers =
  let
    maybeBuffer :: Maybe AudioBuffer
    maybeBuffer = (getString "url" attMap) >>= (\url -> lookup url buffers)
  in
    case maybeBuffer of
      Nothing ->
        trace "buffer not loaded" \_ ->
        pure unit
      Just buffer ->
        trace "buffer loaded" \_ ->
        setBuffer buffer audioBufferNode

setAudioBufferLoopAttr :: ∀ eff. AudioBufferSourceNode-> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setAudioBufferLoopAttr node map =
  case getBool "loop" map of
    Just b ->
      setLoop b node
    _ ->
      pure unit

setAudioBufferLoopStartAttr :: ∀ eff. AudioBufferSourceNode-> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setAudioBufferLoopStartAttr node map =
  case getNumber "setLoopStart" map of
    Just n ->
      setLoopStart n node
    _ ->
      pure unit

setAudioBufferLoopEndAttr :: ∀ eff. AudioBufferSourceNode-> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setAudioBufferLoopEndAttr node map =
  case getNumber "setLoopEnd" map of
    Just n ->
      setLoopEnd n node
    _ ->
      pure unit

setConvolverBufferAttr :: ∀ eff. ConvolverNode -> AttributeMap -> AudioBuffers -> Eff ( wau :: WebAudio | eff) Unit
setConvolverBufferAttr convolverNode attMap buffers =
  let
    maybeBuffer :: Maybe AudioBuffer
    maybeBuffer = (getString "url" attMap) >>= (\url -> lookup url buffers)
  in
    case maybeBuffer of
      Nothing ->
        trace "convolver buffer not loaded" \_ ->
        pure unit
      Just buffer ->
        trace "convolver buffer loaded" \_ ->
        Convolver.setBuffer buffer convolverNode

setConvolverNormalizeAttr :: ∀ eff. ConvolverNode-> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setConvolverNormalizeAttr node map =
  case getBool "normalize" map of
    Just b ->
      Convolver.normalize b node
    _ ->
      pure unit

-- | set a list of audio parameters
setParams :: ∀ eff. Number -> AudioParam -> List AudioParamDef -> Eff ( wau :: WebAudio | eff) Unit
setParams startTime param paramDefs =
  traverse_ (setParam startTime param) paramDefs

-- | set an audio parameter
setParam :: ∀ eff. Number -> AudioParam -> AudioParamDef -> Eff ( wau :: WebAudio | eff) Number
setParam startTime param paramDef =
  case paramDef of
    SetValue n ->
      n <$ setValue n param
    SetValueAtTime n (Absolute t) ->
      setValueAtTime n t param
    SetValueAtTime n (Relative t) ->
      setValueAtTime n (startTime + t) param
    LinearRampToValueAtTime n (Absolute t)->
      linearRampToValueAtTime n t param
    LinearRampToValueAtTime n (Relative t)->
      linearRampToValueAtTime n (startTime + t) param
    ExponentialRampToValueAtTime n (Absolute t) ->
      exponentialRampToValueAtTime n t param
    ExponentialRampToValueAtTime n (Relative t) ->
      exponentialRampToValueAtTime n (startTime + t) param

-- sets of node attributes

setOscillatorAttributes :: ∀ eff. Number -> OscillatorNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setOscillatorAttributes startTime osc map = do
  _ <- setOscillatorTypeAttr osc map
  _ <- setOscillatorDetuneAttr startTime osc map
  setOscillatorFrequencyAttr startTime osc map

setAudioBufferSourceAttributes :: ∀ eff. AudioBufferSourceNode -> AttributeMap -> AudioBuffers -> (Eff (wau :: WebAudio | eff) Unit)
setAudioBufferSourceAttributes node map buffers = do
  _ <- setAudioBufferAttr node map buffers
  _ <- setAudioBufferLoopAttr node map
  _ <- setAudioBufferLoopStartAttr node map
  setAudioBufferLoopEndAttr node map

setGainAttributes :: ∀ eff. Number -> GainNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setGainAttributes startTime gain map = do
  setGainAttr startTime gain map

setDelayAttributes :: ∀ eff. Number -> DelayNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setDelayAttributes startTime delayNode map = do
  setDelayAttr startTime delayNode map

setStereoPannerAttributes :: ∀ eff. Number -> StereoPannerNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setStereoPannerAttributes startTime stereoPannerNode map = do
  setStereoPannerAttr startTime stereoPannerNode map

setDynamicsCompressorAttributes :: ∀ eff. Number -> DynamicsCompressorNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setDynamicsCompressorAttributes startTime dynamicsCompressorNode map = do
  -- setCompressorThresholdAttr startTime dynamicsCompressorNode map
  _ <- setCompressorParam startTime dynamicsCompressorNode "threshold" threshold map
  _ <- setCompressorParam startTime dynamicsCompressorNode "knee" knee map
  _ <- setCompressorParam startTime dynamicsCompressorNode "ratio" ratio map
  _ <- setCompressorParam startTime dynamicsCompressorNode "attack" attack map
  _ <- setCompressorParam startTime dynamicsCompressorNode "release" release map
  pure unit

setConvolverAttributes :: ∀ eff. ConvolverNode -> AttributeMap -> AudioBuffers -> (Eff (wau :: WebAudio | eff) Unit)
setConvolverAttributes node map buffers = do
  _ <- setConvolverBufferAttr node map buffers
  setConvolverNormalizeAttr node map

setBiquadFilterAttributes :: ∀ eff. Number -> BiquadFilterNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setBiquadFilterAttributes startTime bqf map = do
  _ <- setBiquadFilterTypeAttr bqf map
  _ <- setBiquadFilterQualityAttr startTime bqf map
  setBiquadFilterFrequencyAttr startTime bqf map
