module Audio.Graph
 (NodeType(..), NodeDef(..),
  AudioGraph,  Assemblage, AudioBuffers) where

-- | Audio Graph data type

import Audio.WebAudio.Types (AudioNode, AudioBuffer)
import Audio.Graph.Attributes (AttributeMap)
import Data.Map (Map)
import Data.List (List)
import Data.Set (Set)


-- | the type of Audio node.
data NodeType =
   OscillatorType
 | AudioBufferSourceType
 | GainType
 | BiquadFilterType

-- | An audio node definition
data NodeDef = NodeDef
    { nodeType :: NodeType             -- the node type
    , id ::  String                    -- its identity
    , attributes :: AttributeMap       -- its attributes
    , connections :: Set String        -- its connections to other modes
    }

-- | A full graph
type AudioGraph = List NodeDef

-- | A run-time assemblage of nodes built from the graph
type Assemblage = Map String AudioNode

-- | the set of audio buffers identified by any AudioBuffrSourceNode
type AudioBuffers = Map String AudioBuffer
