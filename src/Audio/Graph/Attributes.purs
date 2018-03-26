module Audio.Graph.Attributes
  (AudioAttribute, AttributeMap, AudioParamDef(..),
   addOscillatorType, addFrequency,
   getOscillatorType, getNumber, getString, biquadFilterTypeAttr,
   oscillatorTypeAttr, numberAttr, stringAttr, boolAttr, audioParamsAttr,
   setOscillatorAttributes, setAudioBufferSourceAttributes,
   setGainAttributes, setDelayAttributes, setBiquadFilterAttributes
   ) where

-- | Audio node attributes.  These are either simple or consist of
-- | AudioParams (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam)
-- | which have special properties

import Prelude (Unit, ($), (<$>), (<$), (#), (>>=), id, bind, pure, unit)
import Control.Monad.Eff (Eff)
import Audio.WebAudio.Types (WebAudio, OscillatorNode, GainNode, BiquadFilterNode,
  AudioBufferSourceNode, DelayNode, AudioParam, AudioBuffer)
import Audio.WebAudio.Oscillator (OscillatorType, detune, frequency, setOscillatorType)
import Audio.WebAudio.BiquadFilterNode (BiquadFilterType, setFilterType,
   filterFrequency, quality)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.AudioParam (setValue, setValueAtTime,
  linearRampToValueAtTime, exponentialRampToValueAtTime)
import Audio.WebAudio.AudioBufferSourceNode (setBuffer, setLoop, setLoopStart, setLoopEnd)
import Audio.WebAudio.DelayNode (delayTime)
import Audio.Buffer (AudioBuffers)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..))
import Data.Symbol (SProxy(..))
import Data.Foldable (traverse_)
import Data.Variant (Variant, inj, on, default)

import Debug.Trace (trace)

-- | the (type of) an attribute of an audio node
type AudioAttribute = Variant ( oscillatorType :: OscillatorType
                              , biquadFilterType :: BiquadFilterType
                              , number :: Number
                              , string :: String
                              , bool :: Boolean
                              , audioParams :: List AudioParamDef)

-- | a map of a set of such (named) attributes
type AttributeMap = Map String AudioAttribute

-- | an AudioParam definition
-- | see https://developer.mozilla.org/en-US/docs/Web/API/AudioParam
data AudioParamDef =
    SetValue Number
  | SetValueAtTime Number Number
  | LinearRampToValueAtTime Number Number
  | ExponentialRampToValueAtTime Number Number

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
  fromMaybe Nil $ getVAudioParams <$> (lookup attName map)

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
setOscillatorFrequencyAttr :: ∀ eff. OscillatorNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setOscillatorFrequencyAttr osc map =
  case getAudioParams "frequency" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- frequency osc
        setParams audioParam ps

-- | set the oscillator detune parameter
setOscillatorDetuneAttr :: ∀ eff. OscillatorNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setOscillatorDetuneAttr osc map =
  case getAudioParams "detune" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- detune osc
        setParams audioParam ps


-- | set the biquad filter frequency
setBiquadFilterFrequencyAttr :: ∀ eff. BiquadFilterNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setBiquadFilterFrequencyAttr bqf map =
  case getAudioParams "frequency" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- filterFrequency bqf
        setParams audioParam ps

-- | set the biquad filter quality
setBiquadFilterQualityAttr :: ∀ eff. BiquadFilterNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setBiquadFilterQualityAttr bqf map =
  case getAudioParams "quality" map of
    Nil ->
      pure unit
    ps ->
      do
        audioParam <- quality bqf
        setParams audioParam ps


setGainAttr :: ∀ eff. GainNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setGainAttr gainNode map =
  case getAudioParams "gain" map of
    Nil ->
      pure unit
    ps ->
      do
        gainParam <- gain gainNode
        setParams gainParam ps

setDelayAttr :: ∀ eff. DelayNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setDelayAttr delayNode map =
  case getAudioParams "delayTime" map of
    Nil ->
      pure unit
    ps ->
      do
        delayParam <- delayTime delayNode
        setParams delayParam ps

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

-- | set a list of audio parameters
setParams :: ∀ eff. AudioParam -> List AudioParamDef -> Eff ( wau :: WebAudio | eff) Unit
setParams param paramDefs =
  traverse_ (setParam param) paramDefs

-- | set an audio parameter
setParam :: ∀ eff. AudioParam -> AudioParamDef -> Eff ( wau :: WebAudio | eff) Number
setParam param paramDef =
  case paramDef of
    SetValue n ->
      n <$ setValue n param
    SetValueAtTime n t ->
      setValueAtTime n t param
    LinearRampToValueAtTime n t ->
      linearRampToValueAtTime n t param
    ExponentialRampToValueAtTime n t ->
      exponentialRampToValueAtTime n t param

-- sets of node attributes

setOscillatorAttributes :: ∀ eff. OscillatorNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setOscillatorAttributes osc map = do
  _ <- setOscillatorTypeAttr osc map
  _ <- setOscillatorDetuneAttr osc map
  setOscillatorFrequencyAttr osc map

setAudioBufferSourceAttributes :: ∀ eff. AudioBufferSourceNode -> AttributeMap -> AudioBuffers -> (Eff (wau :: WebAudio | eff) Unit)
setAudioBufferSourceAttributes node map buffers = do
  _ <- setAudioBufferAttr node map buffers
  _ <- setAudioBufferLoopAttr node map
  _ <- setAudioBufferLoopStartAttr node map
  setAudioBufferLoopEndAttr node map

setGainAttributes :: ∀ eff. GainNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setGainAttributes gain map = do
  setGainAttr gain map

setDelayAttributes :: ∀ eff. DelayNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setDelayAttributes delayNode map = do
  setDelayAttr delayNode map

setBiquadFilterAttributes :: ∀ eff. BiquadFilterNode -> AttributeMap -> (Eff (wau :: WebAudio | eff) Unit)
setBiquadFilterAttributes bqf map = do
  _ <- setBiquadFilterTypeAttr bqf map
  _ <- setBiquadFilterQualityAttr bqf map
  setBiquadFilterFrequencyAttr bqf map
