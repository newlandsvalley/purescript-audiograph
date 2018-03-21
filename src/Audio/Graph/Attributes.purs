module Audio.Graph.Attributes
  (AudioAttribute, AttributeMap, AudioParamDef(..),
   addOscillatorType, addFrequency,
   getOscillatorType, getNumber,
   setOscillatorTypeAttr, setFrequencyAttr, setGainAttr,
   oscillatorTypeAttr, numberAttr, audioParamsAttr
   ) where

-- | Audio node attributes.  These are either simple or consist of
-- | AudioParams (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam)
-- | which have special properties

import Prelude (Unit, ($), (<$>), (<$), (#), (>>=), id, bind, pure, unit)
import Control.Monad.Eff (Eff)
import Audio.WebAudio.Types (WebAudio, OscillatorNode, GainNode, AudioParam)
import Audio.WebAudio.Oscillator (OscillatorType, setFrequency, setOscillatorType)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.AudioParam (setValue, getValue, setValueAtTime,
  linearRampToValueAtTime, exponentialRampToValueAtTime, cancelScheduledValues)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..))
import Data.Symbol (SProxy(..))
import Data.Foldable (traverse_)
import Data.Variant (Variant, inj, on, default)

-- | an attribute of an audio node
type AudioAttribute = Variant (oscillatorType :: OscillatorType, number :: Number, audioParams :: List AudioParamDef)

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
_number = SProxy :: SProxy "number"
_audioParams = SProxy :: SProxy "audioParams"

-- variant builders
oscillatorTypeAttr :: OscillatorType -> AudioAttribute
oscillatorTypeAttr t =
  inj _oscillatorType t

numberAttr :: Number -> AudioAttribute
numberAttr t =
  inj _number t

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

-- | get a named Number attribute
getNumber :: String -> AttributeMap -> Maybe Number
getNumber attName map =
  (lookup attName map) >>= getVNumber


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

setFrequencyAttr :: ∀ eff. OscillatorNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setFrequencyAttr osc map =
  case getNumber "frequency" map of
    Just t ->
      setFrequency t osc
    _ ->
      pure unit

setGainAttr :: ∀ eff. GainNode -> AttributeMap -> Eff ( wau :: WebAudio | eff) Unit
setGainAttr gainNode map =
  case getAudioParams "gain" map of
    Nil ->
      pure unit
    ps ->
      do
        gainParam <- gain gainNode
        setParams gainParam ps

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
