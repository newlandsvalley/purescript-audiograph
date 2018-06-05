module Audio.Graph.ResourceLoader (loadBuffers) where

-- | load and decode URL resources representing audio buffers

import Audio.Graph (AudioGraph, NodeDef(..), NodeType(..))
import Audio.Buffer (AudioBuffers)
import Audio.Graph.Attributes (getString)
import Audio.WebAudio.BaseAudioContext (decodeAudioDataAsync)
import Audio.WebAudio.Types (AudioContext, AudioBuffer)
import Effect.Aff (Aff)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.List (List(..), concat, singleton)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.Affjax.Response as Response
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (bind, pure, ($), (<$>), (<<<), (==), (<>))


-- | attempt to load the sound buffers for all nodes that identify them
-- | (currently AudioBufferSourceNodes) and return an Error if any fail
-- | or an audioBuffers map if all's OK.

loadBuffers ::
  AudioContext
  -> AudioGraph
  -> Aff (Either String AudioBuffers)
loadBuffers ctx graph = do
  ebuffs <- (sequence <<< concat) <$> traverse (loadNodeBuffer ctx) graph
  pure $ either Left (Right <<< fromFoldable) ebuffs

-- | nodes with resources that are attempted to be loaded are returned
-- | as a singletom List (either success or failure)
-- | whereas nodes without resources are returned as Nil
loadNodeBuffer ::
  AudioContext
  -> NodeDef
  -> Aff (List (Either String (Tuple String AudioBuffer)))
loadNodeBuffer ctx (NodeDef nd) =
  case nd.nodeType of
    AudioBufferSourceType ->
      do
        singleton <$> loadBufferUrl ctx (NodeDef nd)
    ConvolverType ->
      do
        singleton <$> loadBufferUrl ctx (NodeDef nd)
    _ -> pure $ Nil

loadBufferUrl ::
  AudioContext
  -> NodeDef
  -> Aff (Either String (Tuple String AudioBuffer))
loadBufferUrl ctx (NodeDef nd) =
  case getString "url" nd.attributes of
    Nothing ->
      pure $ Left "no URL attribute"
    Just url ->
      loadSoundBuffer ctx url

-- | load a single sound buffer resource and decode it
loadSoundBuffer ::
  AudioContext
  -> String
  -> Aff (Either String (Tuple String AudioBuffer))
loadSoundBuffer ctx url = do
  res <- affjax Response.arrayBuffer $ defaultRequest { url = url, method = Left GET }
  if (res.status == StatusCode 200)
    then do
      buf <- decodeAudioDataAsync ctx res.response
      pure $ Right (Tuple url buf)
    else
      pure $ Left ("resource: " <> url <> " not found")
