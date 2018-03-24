module Audio.Graph.Compiler (compile) where

-- | The compiler parses and then performs sematic checks

import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.List (List, length)
import Data.Set (Set, empty, insert, member, toUnfoldable)
import Data.Foldable (foldl, intercalate)
import Audio.Graph.Parser (SymbolTable, parse)
import Audio.Graph (AudioGraph, NodeDef(..))
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

-- check a node for identifier erros
checkNode :: SymbolTable -> ErrorSet -> NodeDef -> ErrorSet
checkNode st errors (NodeDef nd) =
  checkConnectionIds st errors nd.connections

-- add any identifiers not in the symbol table to the result
checkConnectionIds :: SymbolTable -> ErrorSet -> Set String -> ErrorSet
checkConnectionIds st errors connections =
  --- (b -> a -> b) -> b -> f a -> b
  foldl (checkConnectionId st) errors connections

-- check that an identifer reference is defined somewhere as a done in the graph
-- (i.e. is in the node names in the symbol table)
checkConnectionId :: SymbolTable -> ErrorSet -> String -> ErrorSet
checkConnectionId st errors identifier =
  if (member identifier st.nodeNames) then
    errors
  else
    insert identifier errors
