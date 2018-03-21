module Audio.Graph.Parser (parse) where

-- | Parse a web-audio-graph DSL

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List (singleton) as L
import Data.Map (empty, fromFoldable)
import Data.Set (Set, fromFoldable, insert, member, singleton) as Set
import Data.Int (toNumber)
import Data.Tuple (Tuple(..), fst)
import Prelude (pure, show, ($), (*>), (<$), (<$>), (<*), (<*>), (<<<), (<>), (==), (>>=))
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.Combinators (choice, many, sepBy1, (<?>))
import Text.Parsing.StringParser.String (string, regex, skipSpaces)
import Text.Parsing.StringParser.Num (numberOrInt, unsignedInt)
import Audio.Graph (AudioGraph, NodeType(..), NodeDef(..))
import Audio.Graph.Attributes (AudioAttribute, AttributeMap, AudioParamDef(..),
    oscillatorTypeAttr, numberAttr, audioParamsAttr, biquadFilterTypeAttr)
import Audio.WebAudio.Oscillator (readOscillatorType)
import Audio.WebAudio.BiquadFilterNode (readBiquadFilterType)


type SymbolTable =
  { nodeNames :: Set.Set String
  }

-- there is always one implied node named output
initialSymbolTable :: SymbolTable
initialSymbolTable =
  { nodeNames : Set.singleton("output") }


audioNodes :: SymbolTable -> Parser (Tuple AudioGraph SymbolTable)
audioNodes st =
  audioNode st >>= (\state -> moreNodesOrEnd state)

moreNodesOrEnd :: Tuple NodeDef SymbolTable -> Parser (Tuple AudioGraph SymbolTable)
moreNodesOrEnd (Tuple lastNode st) =
  buildNodeList (Tuple lastNode st) <$>
    choice
      [ endOfNodes st
      , audioNodes st
      ]

-- parse any legitimate audio node and return it alongside the
-- new symbol table that now also contains its id
-- the POC just demonstrated oscillator and gain
audioNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
audioNode st =
  choice
    [
      oscillatorNode st
    , gainNode st
    , biquadFilterNode st
    ]
      <?> "audio node"

oscillatorNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
oscillatorNode st =
  buildNode <$> oscillatorNodeType <*> nodeId st <*> oscillatorAttributes <*> connections st

gainNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
gainNode st =
  buildNode <$> gainNodeType <*> nodeId st <*> gainAttributes <*> connections st

biquadFilterNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
biquadFilterNode st =
  buildNode <$> biquadFilterNodeType <*> nodeId st <*> biquadFilterAttributes <*> connections st


oscillatorNodeType :: Parser NodeType
oscillatorNodeType =
  OscillatorType <$ keyWord "Oscillator"

gainNodeType :: Parser NodeType
gainNodeType =
  GainType <$ keyWord "Gain"

biquadFilterNodeType :: Parser NodeType
biquadFilterNodeType =
  BiquadFilterType <$ keyWord "BiquadFilter"

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
noAttributes :: Parser AttributeMap
noAttributes =
  (pure empty) <$
      openCurlyBracket <*> closeCurlyBracket

-- gain attributes

-- at the moment we require a gain attribute, nothing more
gainAttributes :: Parser AttributeMap
gainAttributes =
  (fromFoldable <<< L.singleton) <$>
    (openCurlyBracket *> gainAttribute <* closeCurlyBracket)

gainAttribute :: Parser (Tuple String AudioAttribute)
gainAttribute =
  Tuple <$> keyWord "gain" <*> audioParams

-- oscillator attributes

oscillatorAttributes :: Parser AttributeMap
oscillatorAttributes =
  fromFoldable <$>
    (openCurlyBracket *> oscillatorAttributeList <* closeCurlyBracket)

oscillatorAttributeList :: Parser (List (Tuple String AudioAttribute))
oscillatorAttributeList =
  many
    (choice
      [
        oscillatorTypeAttribute
      , frequency
      ]
    )

oscillatorTypeAttribute :: Parser (Tuple String AudioAttribute)
oscillatorTypeAttribute =
  Tuple <$> keyWord "type" <*> oscillatorType

oscillatorType :: Parser AudioAttribute
oscillatorType =
  (oscillatorTypeAttr <<< readOscillatorType) <$>
    choice
      [
        keyWord "sine"
      , keyWord "square"
      , keyWord "sawtooth"
      , keyWord "triangle"
      , keyWord "custom"
      ]
        <?> "oscillator type"

frequency :: Parser (Tuple String AudioAttribute)
frequency =
  Tuple <$> keyWord "frequency" <*> audioParams
    <?> "frequency"

biquadFilterAttributes :: Parser AttributeMap
biquadFilterAttributes =
  fromFoldable <$>
    (openCurlyBracket *> biquadFilterAttributeList <* closeCurlyBracket)

biquadFilterAttributeList :: Parser (List (Tuple String AudioAttribute))
biquadFilterAttributeList =
  many
    (choice
      [
        biquadFilterTypeAttribute
      , frequency
      ]
    )

biquadFilterTypeAttribute :: Parser (Tuple String AudioAttribute)
biquadFilterTypeAttribute =
  Tuple <$> keyWord "type" <*> biquadFilterType

biquadFilterType :: Parser AudioAttribute
biquadFilterType =
  (biquadFilterTypeAttr <<< readBiquadFilterType) <$>
    choice
      [
        keyWord "lowpass"
      , keyWord "highpass"
      , keyWord "bandpass"
      , keyWord "lowshelf"
      , keyWord "highshelf"
      , keyWord "peaking"
      , keyWord "notch"
      , keyWord "allpass"
      ]
        <?> "biquad filter type"

-- general audio params

audioParams :: Parser AudioAttribute
audioParams =
  audioParamsAttr <$> (fullAudioParams <|> simpleAudioParam)

-- a full set of audio parameters is one or more parameters
-- separated by commas and framed by square brackets
fullAudioParams :: Parser (List AudioParamDef)
fullAudioParams =
  openBracket *>  sepBy1 audioParam comma <* closeBracket

-- but we also support s simple version which is just a number and is
-- entirely equivalent to a singleton list of one SetValue parameter
simpleAudioParam :: Parser (List AudioParamDef)
simpleAudioParam =
  (L.singleton <<< SetValue) <$> number

audioParam :: Parser AudioParamDef
audioParam =
  choice
    [
      setValueAtTime  -- must come before setValue to remove ambiguity
    , setValue
    , linearRampToValueAtTime
    , exponentialRampToValueAtTime
    ]

setValue :: Parser AudioParamDef
setValue =
  SetValue <$> ((keyWord "setValue") *> number)

setValueAtTime :: Parser AudioParamDef
setValueAtTime =
  SetValueAtTime <$> ((keyWord "setValueAtTime") *>
    number) <*> number
    <?> "setValueAtTime"

linearRampToValueAtTime :: Parser AudioParamDef
linearRampToValueAtTime  =
  LinearRampToValueAtTime <$> ((keyWord "linearRampToValueAtTime") *>
    number) <*> number
    <?> "linearRampToValueAtTime"

exponentialRampToValueAtTime :: Parser AudioParamDef
exponentialRampToValueAtTime  =
  ExponentialRampToValueAtTime  <$> ((keyWord "exponentialRampToValueAtTime") *>
    number) <*> number
    <?> "exponentialRampToValueAtTime"

-- low level parsers

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

endOfNodes :: SymbolTable -> Parser (Tuple AudioGraph SymbolTable)
endOfNodes st =
  Tuple Nil st <$ string "End"

comma :: Parser String
comma =
  string "," <* skipSpaces

number :: Parser Number
number =
  numberOrInt <* skipSpaces

intAttribute :: Parser AudioAttribute
intAttribute =
  (numberAttr <<< toNumber) <$> unsignedInt <* skipSpaces

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

buildNode :: NodeType -> Tuple String SymbolTable -> AttributeMap -> Set.Set String -> Tuple NodeDef SymbolTable
buildNode nodeType (Tuple id st) attributes connections =
  Tuple (NodeDef{ nodeType, id, attributes, connections}) st

buildNodeList :: Tuple NodeDef SymbolTable -> Tuple (List NodeDef) SymbolTable -> Tuple AudioGraph SymbolTable
buildNodeList (Tuple n _) (Tuple ns st) =
  Tuple (n : ns) st

-- | Parse an audio graph
parse :: String -> Either String AudioGraph
parse s =
  case runParser (audioNodes initialSymbolTable) s of
    Right n ->
      Right (fst n)

    Left e ->
      Left $ show e
