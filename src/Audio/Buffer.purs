module Audio.Buffer (AudioBuffers) where

import Data.Map (Map)
import Audio.WebAudio.Types (AudioBuffer)

-- | the set of audio buffers identified by any AudioBuffrSourceNode
type AudioBuffers = Map String AudioBuffer
