module Audio.Graph
 (NodeType(..), NodeDef(..),
  AudioGraph,  Assemblage) where

-- | Audio Graph data type

import Audio.WebAudio.Types (AudioNode)
import Audio.Graph.Attributes (AttributeMap)
import Data.Map (Map)
import Data.List (List)
import Data.Set (Set)


-- | the type of Audio node.
-- | in the POC we only support these two
data NodeType =
   OscillatorType
 | GainType

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
