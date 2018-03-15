module Audio.Graph.Parser (AudioAttributes, AudioAttribute, AudioParam, NodeType(..), Node(..), parse) where


import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List (singleton) as L
import Data.Map (Map, empty, fromFoldable)
import Data.Set (Set, fromFoldable, insert, member, singleton) as Set
import Data.Tuple (Tuple(..), fst)
import Prelude (Unit, pure, show, ($), (*), (*>), (<$), (<$>), (<*), (<*>), (<<<), (<>), (==), (>>=))
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos, fail, runParser)
import Text.Parsing.StringParser.Combinators (choice, many1, optionMaybe, sepBy1, (<?>))
import Text.Parsing.StringParser.String (string, regex, skipSpaces)
import Text.Parsing.StringParser.Num (numberOrInt)

-- | an AudioParam
-- | see https://developer.mozilla.org/en-US/docs/Web/API/AudioParam
data AudioParam =
    SetValue Number
  | SetValueAtTime Number Number
  | LinearRampToValueAtTime Number Number
  | ExponentialRampToValueAtTime Number Number

-- | an Audio Attribute represents the range of types that a node attribute may take
data AudioAttribute =
    ANum Number
  | AParam (List AudioParam)

-- | a mapping of attribute name to value
type AudioAttributes = Map String AudioAttribute

-- | the type of Audio node.
-- | in the POC we only support these two
data NodeType =
   Oscillator
 | Gain

-- | An audio node
data Node = Node
    { node :: NodeType                 -- the node type
    , id ::  String                    -- its identity
    , attributes :: AudioAttributes    -- its attributes
    , connections :: Set.Set String    -- its connections to other modes
    }

type SymbolTable =
  { nodeNames :: Set.Set String
  }

-- there is always one implied node named output
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

-- parse any legitimate audio node and return it alongside the
-- new symbol table that now also contains its id
-- the POC just demonstrated oscillator and gain
audioNode :: SymbolTable -> Parser (Tuple Node SymbolTable)
audioNode st =
  choice
    [
      oscillatorNode st
    , gainNode st
    ]

oscillatorNode :: SymbolTable -> Parser (Tuple Node SymbolTable)
oscillatorNode st =
  buildNode <$> oscillatorType <*> nodeId st <*> attributes <*> connections st

gainNode :: SymbolTable -> Parser (Tuple Node SymbolTable)
gainNode st =
  buildNode <$> gainType <*> nodeId st <*> gainAttributes <*> connections st

oscillatorType :: Parser NodeType
oscillatorType =
    Oscillator <$ keyWord "Oscillator"

gainType :: Parser NodeType
gainType =
    Gain <$ keyWord "Gain"

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

-- audio params

-- placeholder only
attributes :: Parser AudioAttributes
attributes =
  pure empty

-- at the moment we require a gain attribute, nothing more
gainAttributes :: Parser AudioAttributes
gainAttributes =
  (fromFoldable <<< L.singleton) <$>
    (openCurlyBracket *> gainAttribute <* closeCurlyBracket)

gainAttribute :: Parser (Tuple String AudioAttribute)
gainAttribute =
  Tuple <$> keyWord "gain" <*> audioParams

audioParams :: Parser AudioAttribute
audioParams =
  AParam <$> (fullAudioParams <|> simpleAudioParam)

-- a full set of audio parameters is one or more parameters
-- separated by commas and framed by square brackets
fullAudioParams :: Parser (List AudioParam)
fullAudioParams =
  openBracket *>  sepBy1 audioParam comma <* closeBracket

-- but we also support s simple version which is just a number and is
-- entirely equivalent to a singleton list of one SetValue parameter
simpleAudioParam :: Parser (List AudioParam)
simpleAudioParam =
  (L.singleton <<< SetValue) <$> number

audioParam :: Parser AudioParam
audioParam =
  choice
    [
      setValueAtTime  -- must come before setValue to remove ambiguity
    , setValue
    , linearRampToValueAtTime
    , exponentialRampToValueAtTime
    ]

setValue :: Parser AudioParam
setValue =
  SetValue <$> ((keyWord "setValue") *> number)

setValueAtTime :: Parser AudioParam
setValueAtTime =
  SetValueAtTime <$> ((keyWord "setValueAtTime") *>
    number) <*> number
    <?> "setValueAtTime"

linearRampToValueAtTime :: Parser AudioParam
linearRampToValueAtTime  =
  LinearRampToValueAtTime <$> ((keyWord "linearRampToValueAtTime") *>
    number) <*> number
    <?> "linearRampToValueAtTime"

exponentialRampToValueAtTime :: Parser AudioParam
exponentialRampToValueAtTime  =
  ExponentialRampToValueAtTime  <$> ((keyWord "exponentialRampToValueAtTime") *>
    number) <*> number
    <?> "exponentialRampToValueAtTime"



keyWord :: String -> Parser String
keyWord s =
  string s <* skipSpaces

openBracket :: Parser String
openBracket =
  string "[" <* skipSpaces

closeBracket :: Parser String
closeBracket =
  string "]" <* skipSpaces

openCurlyBracket :: Parser String
openCurlyBracket =
  string "{" <* skipSpaces

closeCurlyBracket :: Parser String
closeCurlyBracket =
  string "}" <* skipSpaces

endOfNodes :: SymbolTable -> Parser (Tuple (List Node) SymbolTable)
endOfNodes st =
  Tuple Nil st <$ string "End"

comma :: Parser String
comma =
  string "," <* skipSpaces

number :: Parser Number
number =
  numberOrInt <* skipSpaces

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

buildNode :: NodeType -> Tuple String SymbolTable -> AudioAttributes -> Set.Set String -> Tuple Node SymbolTable
buildNode node (Tuple id st) attributes connections =
  Tuple (Node { node, id, attributes, connections}) st

buildNodeList :: Tuple Node SymbolTable -> Tuple (List Node) SymbolTable -> Tuple (List Node) SymbolTable
buildNodeList (Tuple n _) (Tuple ns st) =
  Tuple (n : ns) st

-- | Parse an audio graph
parse :: String -> Either String (List Node)
parse s =
  case runParser (audioNodes initialSymbolTable) s of
    Right n ->
      Right (fst n)

    Left e ->
      Left $ show e
