module Audio.Graph
 (NodeId, NodeType(..), NodeDef(..), Reference(..),
  AudioGraph,  Assemblage) where

-- | Audio Graph data type

import Audio.WebAudio.Types (AudioNode)
import Audio.Graph.Attributes (AttributeMap)
import Data.Map (Map)
import Data.List (List)
import Data.Set (Set)
import Prelude (class Eq, class Ord)

-- | an identifier of a node
type NodeId = String

-- | the type of Audio node.
data NodeType =
   OscillatorType
 | AudioBufferSourceType
 | GainType
 | BiquadFilterType
 | DelayType
 | StereoPannerType
 | DynamicsCompressorType

-- | a reference
data Reference =
   NodeRef NodeId                -- refer to a node
 | ParameterRef NodeId String    -- refer to an audio parameter

derive instance eqReference :: Eq Reference
derive instance ordReference :: Ord Reference

-- | An audio node definition
data NodeDef = NodeDef
    { nodeType :: NodeType             -- the node type
    , id :: NodeId                     -- its identity
    , attributes :: AttributeMap       -- its attributes
    , connections :: Set Reference     -- its connections to other modes
    }

-- | A full graph
type AudioGraph = List NodeDef

-- | A run-time assemblage of nodes built from the graph
type Assemblage = Map String AudioNode
