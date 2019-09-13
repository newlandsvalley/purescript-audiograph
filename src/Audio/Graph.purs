module Audio.Graph
 (NodeId, NodeType(..), NodeDef(..), NodeDefs, Reference(..),
  AudioGraph,  AssembledNodes, Assemblage, ListenerDef) where

-- | Audio Graph data type

import Audio.Graph.Attributes (AttributeMap)
import Audio.WebAudio.Types (AudioNode, AudioListener)
import Data.List (List)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (Maybe)
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
 | ConvolverType
 | PannerType

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

data ListenerDef = ListenerDef
  { attributes :: AttributeMap }


type NodeDefs = List NodeDef

-- | A full graph
type AudioGraph =
  { nodeDefs :: List NodeDef
  , listener :: Maybe ListenerDef
  }

type AssembledNodes = Map String AudioNode

-- | A run-time assemblage of nodes built from the graph
type Assemblage =
  { nodes :: AssembledNodes
  , listener :: Maybe AudioListener
  }
