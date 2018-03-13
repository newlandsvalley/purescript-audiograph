module Audio.Graph.Parser (NodeType(..), Node(..), parse) where


import Data.List (List(..), (:))
import Data.Set (Set(..), fromFoldable, insert, member, singleton) as Set
import Data.Tuple (Tuple(..), fst)
import Data.Either (Either(..))
import Prelude (Unit, pure, show, ($), (*>), (<$), (<$>), (<*), (<*>), (<>), (=<<), (==), (>>=))
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos, fail, runParser)
import Text.Parsing.StringParser.Combinators (choice, sepBy1, (<?>))
import Text.Parsing.StringParser.String (string, regex, skipSpaces)


data NodeType =
   Oscillator
 | Gain

data Node = Node
    { node :: NodeType
    , id ::  String
    , connections :: Set.Set String
    }

type SymbolTable =
  { nodeNames :: Set.Set String
  }

initialSymbolTable :: SymbolTable
initialSymbolTable =
  { nodeNames : Set.singleton("output") }


audioNodes :: SymbolTable -> Parser (Tuple (List Node) SymbolTable)
audioNodes st =
  audioNode st >>= (\state -> moreNodesOrEnd state)

moreNodesOrEnd :: Tuple Node SymbolTable -> Parser (Tuple (List Node) SymbolTable)
moreNodesOrEnd (Tuple lastNode st) =
  buildNodeList (Tuple lastNode st) <$>
    choice
      [ endOfNodes st
      , audioNodes st
      ]

-- parse any legitimate audio node and return it a longside the
-- new symbol table that now also contains its id
audioNode :: SymbolTable -> Parser (Tuple Node SymbolTable)
audioNode st =
  choice
    [
      oscillatorNode st
    , gainNode st
    ]

oscillatorNode :: SymbolTable -> Parser (Tuple Node SymbolTable)
oscillatorNode st =
  buildNode <$> oscillatorType <*> nodeId st <*> connections st

gainNode :: SymbolTable -> Parser (Tuple Node SymbolTable)
gainNode st =
  buildNode <$> gainType <*> nodeId st <*> connections st

oscillatorType :: Parser NodeType
oscillatorType =
    Oscillator <$ string "Oscillator" <* skipSpaces

gainType :: Parser NodeType
gainType =
    Gain <$ string "Gain" <* skipSpaces

nodeId :: SymbolTable -> Parser (Tuple String SymbolTable)
nodeId st =
  identifier >>= (\id -> checkValidNodeId st id)


identifier :: Parser String
identifier = regex "[a-z][a-zA-Z0-9]*" <* skipSpaces

connections :: SymbolTable -> Parser (Set.Set String)
connections st =
  Set.fromFoldable <$> (openBracket *> connectionList st <* closeBracket)

connectionList :: SymbolTable -> Parser (List String)
connectionList st =
  sepBy1 (connection st) (string "," <* skipSpaces)

connection :: SymbolTable -> Parser String
connection st =
  identifier >>= (\id -> checkValidNodeRef st id)



openBracket :: Parser String
openBracket =
  string "[" <* skipSpaces

closeBracket :: Parser String
closeBracket =
  string "]" <* skipSpaces

endOfNodes :: SymbolTable -> Parser (Tuple (List Node) SymbolTable)
endOfNodes st =
  Tuple Nil st <$ string "End"

-- symbol table operations

-- check that an identifier for a new node has not already been used
-- if valid, then add to the symbol table
checkValidNodeId :: SymbolTable -> String -> Parser (Tuple String SymbolTable)
checkValidNodeId st nodeId =
  if (nodeId == "output") then
    fail ("identifier: output is reserved as the default output node")
  else if Set.member nodeId st.nodeNames then
    fail ("identifier: " <> nodeId <> " has already been used")
  else
    let
      nodeNames = Set.insert nodeId st.nodeNames
    in
      pure (Tuple nodeId {nodeNames} )

-- check that a reference to a node already exists
checkValidNodeRef :: SymbolTable -> String -> Parser String
checkValidNodeRef st nodeId =
  if Set.member nodeId st.nodeNames then
    pure nodeId
  else
    fail ("identifier: " <> nodeId <> " has not been defined")

-- builders

buildNode :: NodeType -> Tuple String SymbolTable -> Set.Set String -> Tuple Node SymbolTable
buildNode node (Tuple id st) connections =
  Tuple (Node { node, id, connections}) st

buildNodeList :: Tuple Node SymbolTable -> Tuple (List Node) SymbolTable -> Tuple (List Node) SymbolTable
buildNodeList (Tuple n st) (Tuple ns _) =
  Tuple (n : ns) st

  -- | Parse a normalised MIDI string.
parse :: String -> Either String (List Node)
parse s =
  case runParser (audioNodes initialSymbolTable) s of
    Right n ->
      Right (fst n)

    Left e ->
      Left $ show e
