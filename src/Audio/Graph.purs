module Audio.Graph
 (NodeType(..), NodeDef(..),
  AudioGraph,  Assemblage) where

import Audio.WebAudio.Oscillator (OscillatorType)
import Audio.WebAudio.Types (AudioNode)
import Audio.Graph.Attributes (AudioAttribute, AttributeMap)
import Data.List (List)
import Data.Map (Map)
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

type AudioGraph = List NodeDef

type Assemblage = Map String AudioNode
