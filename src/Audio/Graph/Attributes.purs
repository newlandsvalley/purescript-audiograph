module Audio.Graph.Attributes
  (AudioAttribute, AttributeMap, AudioParam(..),
   addOscillatorType, addFrequency,
   getOscillatorType, getFrequency,
   setOscillatorTypeAttr, setFrequencyAttr,
   oscillatorTypeAttr, numberAttr, audioParamsAttr
   ) where

import Prelude (Unit, (#), (>>=), pure, unit)
import Control.Monad.Eff (Eff)
import Audio.WebAudio.Types (WebAudio, OscillatorNode)
import Audio.WebAudio.Oscillator (OscillatorType, setFrequency, setOscillatorType)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, on, default)

-- | an attribute of an audio node
type AudioAttribute = Variant (oscillatorType :: OscillatorType, number :: Number, audioParams :: List AudioParam)

-- | a map of a set of such (named) attributes
type AttributeMap = Map String AudioAttribute

-- | an AudioParam
-- | see https://developer.mozilla.org/en-US/docs/Web/API/AudioParam
data AudioParam =
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

audioParamsAttr :: List AudioParam -> AudioAttribute
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

getNumber :: AudioAttribute -> Maybe Number
getNumber =
    default Nothing
      # on _number Just

-- get attributes from the map

getOscillatorType :: AttributeMap -> Maybe OscillatorType
getOscillatorType map =
  (lookup "type" map) >>= getOscillatorVal
    where
      getOscillatorVal :: AudioAttribute -> Maybe OscillatorType
      getOscillatorVal =
        default Nothing
          # on _oscillatorType Just

getFrequency :: AttributeMap -> Maybe Number
getFrequency map =
  (lookup "frequency" map) >>= getNumber

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
  case getFrequency map of
    Just t ->
      setFrequency t osc
    _ ->
      pure unit
