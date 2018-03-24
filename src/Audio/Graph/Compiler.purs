module Audio.Graph.Compiler (compile) where

-- | The compiler parses and then performs sematic checks

import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.List (List, length)
import Data.Set (Set, empty, insert, member, toUnfoldable)
import Data.Foldable (foldl, intercalate)
import Audio.Graph.Parser (SymbolTable, parse)
import Audio.Graph (AudioGraph, NodeDef(..), Reference(..))
import Prelude (($), (<>), show)

type ErrorSet = Set String


-- | A compiler which performs both parsing and semantic checks
compile :: String -> Either String AudioGraph
compile s =
  case parse s of
    Right (Tuple graph st)  ->
      semCheck st graph

    Left e ->
      Left $ show e

semCheck :: SymbolTable -> AudioGraph -> Either String AudioGraph
semCheck st graph =
  let
    errors :: List String
    errors = toUnfoldable $ checkGraph st graph
    errorText = intercalate "," errors
  in
    case (length errors) of
      0 ->
        Right graph
      1 ->
        Left ("identifier: " <> errorText <> " has not been defined")
      _ ->
        Left ("identifiers: " <> errorText <> " have not been defined")

-- check a graph for identifier errors
checkGraph :: SymbolTable -> AudioGraph -> ErrorSet
checkGraph st graph =
  let
    errors = empty
  in
    foldl (checkNode st) errors graph

-- check a node for identifier errors in its connections
checkNode :: SymbolTable -> ErrorSet -> NodeDef -> ErrorSet
checkNode st errors (NodeDef nd) =
  checkConnectionIds st errors nd.connections

-- add any identifiers not in the symbol table to the error result set
checkConnectionIds :: SymbolTable -> ErrorSet -> Set Reference -> ErrorSet
checkConnectionIds st errors connections =
  --- (b -> a -> b) -> b -> f a -> b
  foldl (checkConnectionRef st) errors connections

-- a checked reference may be to a node or to an audio parameter
checkConnectionRef :: SymbolTable -> ErrorSet -> Reference -> ErrorSet
checkConnectionRef st errors ref =
  case ref of
    NodeRef nodeId ->
      checkConnectionNode st errors nodeId
    ParameterRef nodeId parameterId ->
      checkConnectionParam st errors nodeId parameterId

-- check that an identifer reference is defined somewhere as a node in the graph
-- (i.e. is in the node names in the symbol table)
checkConnectionNode :: SymbolTable -> ErrorSet -> String -> ErrorSet
checkConnectionNode st errors identifier =
  if (member identifier st.nodeNames) then
    errors
  else
    insert identifier errors

-- check that the node exists and that it has defined the audio param
--
-- We check that the node exists but not that the attribute exists on that node.
-- I'm not sure if we should do this - are we allowed to connect to a node
-- parameter if it implicitly belongs to the node but is not explicitly
-- defined in the graph?
checkConnectionParam :: SymbolTable -> ErrorSet -> String -> String -> ErrorSet
checkConnectionParam st errors nodeId paramId =
  if (member nodeId st.nodeNames) then
    errors
  else
    insert (nodeId <> "." <> paramId) errors
