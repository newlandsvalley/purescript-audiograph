module Audio.Graph
 (AudioAttributes, AudioAttribute(..), AudioParam(..), NodeType(..), NodeDef(..),
  AudioGraph,  Assemblage) where

import Audio.WebAudio.Oscillator (OscillatorType)
import Audio.WebAudio.Types (AudioNode)
import Data.List (List)
import Data.Map (Map)
import Data.Set (Set)

-- | an AudioParam
-- | see https://developer.mozilla.org/en-US/docs/Web/API/AudioParam
data AudioParam =
    SetValue Number
  | SetValueAtTime Number Number
  | LinearRampToValueAtTime Number Number
  | ExponentialRampToValueAtTime Number Number

-- | an Audio Attribute represents the range of types that a node attribute may take
data AudioAttribute =
    ANum Number
  | AParam (List AudioParam)
  | AOscillatorType OscillatorType

-- | a mapping of attribute name to value
type AudioAttributes = Map String AudioAttribute

-- | the type of Audio node.
-- | in the POC we only support these two
data NodeType =
   OscillatorType
 | GainType

-- | An audio node definition
data NodeDef = NodeDef
    { nodeType :: NodeType             -- the node type
    , id ::  String                    -- its identity
    , attributes :: AudioAttributes    -- its attributes
    , connections :: Set String        -- its connections to other modes
    }

type AudioGraph = List NodeDef

type Assemblage = Map String AudioNode
