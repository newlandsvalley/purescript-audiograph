module Audio.Graph.ResourceLoader (loadBuffers) where

-- | load and decode URL resources representing audio buffers

import Audio.Graph (AudioGraph, NodeDef(..), NodeType(..))
import Audio.Buffer (AudioBuffers)
import Audio.Graph.Attributes (getString)
import Audio.WebAudio.AudioContext (decodeAudioDataAsync)
import Audio.WebAudio.Types (WebAudio, AudioContext, AudioBuffer)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.List (List(..), concat, singleton)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (bind, pure, ($), (<$>), (<<<), (==), (<>))


-- | attempt to load the sound buffers for all nodes that identify them
-- | (currently AudioBufferSourceNodes) and return an Error if any fail
-- | or an audioBuffers map if all's OK.

loadBuffers :: ∀ eff.
  AudioContext
  -> AudioGraph
  -> Aff
      ( ajax :: AJAX
      , wau :: WebAudio
      | eff
      )
      (Either String AudioBuffers)
loadBuffers ctx graph = do
  ebuffs <- (sequence <<< concat) <$> traverse (loadNodeBuffer ctx) graph
  let
    bufferMap = either Left (Right <<< fromFoldable) ebuffs
  pure bufferMap

-- | nodes with resources that are attempted to be loaded are returned
-- | as a singletom List (either success or failure)
-- | whereas nodes without resources are returned as Nil
loadNodeBuffer :: ∀ eff.
  AudioContext
  -> NodeDef
  -> Aff
      ( ajax :: AJAX
      , wau :: WebAudio
      | eff
      )
      (List (Either String (Tuple String AudioBuffer)))
loadNodeBuffer ctx (NodeDef nd) =
  case nd.nodeType of
    AudioBufferSourceType ->
      do
        singleton <$> loadAudioSourceBuffer ctx (NodeDef nd)
    _ -> pure $ Nil

loadAudioSourceBuffer :: ∀ eff.
  AudioContext
  -> NodeDef
  -> Aff
      ( ajax :: AJAX
      , wau :: WebAudio
      | eff
      )
      (Either String (Tuple String AudioBuffer))
loadAudioSourceBuffer ctx (NodeDef nd) =
  case getString "url" nd.attributes of
    Nothing ->
      pure $ Left "no URL attribute"
    Just url ->
      loadSoundBuffer ctx url

-- | load a single sound buffer resource and decode it
loadSoundBuffer :: ∀ eff.
  AudioContext
  -> String
  -> Aff
     ( ajax :: AJAX
     , wau :: WebAudio
     | eff
     )
     (Either String (Tuple String AudioBuffer))
loadSoundBuffer ctx url = do
  res <- affjax $ defaultRequest { url = url, method = Left GET }
  if (res.status == StatusCode 200)
    then do
      buf <- decodeAudioDataAsync ctx res.response
      pure $ Right (Tuple url buf)
    else
      pure $ Left ("resource: " <> url <> " not found")
