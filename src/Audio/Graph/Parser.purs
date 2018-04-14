module Audio.Graph.Parser (PositionedParseError(..), SymbolTable, parse) where

-- | Parse a web-audio-graph DSL

import Audio.Graph (AudioGraph, NodeType(..), NodeDef(..), Reference(..))
import Audio.Graph.Attributes (AudioAttribute, AttributeMap, AudioParamDef(..), Time(..),
  oscillatorTypeAttr, numberAttr, stringAttr, boolAttr, audioParamsAttr, biquadFilterTypeAttr)
import Audio.WebAudio.BiquadFilterNode (readBiquadFilterType)
import Audio.WebAudio.Oscillator (readOscillatorType)
import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List (singleton) as L
import Data.Map (empty, fromFoldable)
import Data.Bifunctor (bimap)
import Data.Set (Set, fromFoldable, insert, member, singleton) as Set
import Data.Tuple (Tuple(..))
import Prelude (class Show, pure, show, (*>), (<$), (<$>), (<*), (<*>), (<<<), (<>), (==), (>>=))
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos, fail, try)
import Text.Parsing.StringParser.Combinators (choice, option, sepBy, sepBy1, (<?>))
import Text.Parsing.StringParser.Num (numberOrInt, unsignedInt)
import Text.Parsing.StringParser.String (string, regex, skipSpaces)

-- | the symbol table currently just holds node ids
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
    , audioBufferSourceNode st
    , gainNode st
    , biquadFilterNode st
    , delayNode st
    , stereoPannerNode st
    , dynamicsCompressorNode st
    , convolverNode st
    ]
      <?> "audio node"

oscillatorNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
oscillatorNode st =
  buildNode <$> oscillatorNodeType <*> nodeId st <*> oscillatorAttributes <*> connections

audioBufferSourceNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
audioBufferSourceNode st =
  buildNode <$> audioBufferSourceNodeType <*> nodeId st <*> audioBufferSourceAttributes <*> connections


gainNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
gainNode st =
  buildNode <$> gainNodeType <*> nodeId st <*> gainAttributes <*> connections

biquadFilterNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
biquadFilterNode st =
  buildNode <$> biquadFilterNodeType <*> nodeId st <*> biquadFilterAttributes <*> connections

oscillatorNodeType :: Parser NodeType
oscillatorNodeType =
  OscillatorType <$ keyWord "Oscillator"

audioBufferSourceNodeType :: Parser NodeType
audioBufferSourceNodeType =
  AudioBufferSourceType <$ keyWord "AudioBufferSource"

delayNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
delayNode st =
  buildNode <$> delayNodeType <*> nodeId st <*> delayAttributes <*> connections

gainNodeType :: Parser NodeType
gainNodeType =
  GainType <$ keyWord "Gain"

biquadFilterNodeType :: Parser NodeType
biquadFilterNodeType =
  BiquadFilterType <$ keyWord "BiquadFilter"

delayNodeType :: Parser NodeType
delayNodeType =
  DelayType <$ keyWord "Delay"

stereoPannerNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
stereoPannerNode st =
  buildNode <$> stereoPannerNodeType <*> nodeId st <*> stereoPannerAttributes <*> connections

stereoPannerNodeType :: Parser NodeType
stereoPannerNodeType =
  StereoPannerType <$ keyWord "StereoPanner"

dynamicsCompressorNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
dynamicsCompressorNode st =
  buildNode <$> dynamicsCompressorNodeType <*> nodeId st <*> dynamicsCompressorAttributes <*> connections

dynamicsCompressorNodeType :: Parser NodeType
dynamicsCompressorNodeType =
  DynamicsCompressorType <$ keyWord "DynamicsCompressor"

convolverNode :: SymbolTable -> Parser (Tuple NodeDef SymbolTable)
convolverNode st =
  buildNode <$> convolverNodeType <*> nodeId st <*> convolverAttributes <*> connections

convolverNodeType :: Parser NodeType
convolverNodeType =
  ConvolverType <$ keyWord "Convolver"

nodeId :: SymbolTable -> Parser (Tuple String SymbolTable)
nodeId st =
  identifier >>= (\id -> checkValidNodeId st id)

identifier :: Parser String
identifier = regex "[a-z][a-zA-Z0-9]*" <* skipSpaces

connections :: Parser (Set.Set Reference)
connections =
  Set.fromFoldable <$>
    option Nil
      (openBracket *> connectionList <* closeBracket)

connectionList :: Parser (List Reference)
connectionList =
  sepBy1 connection (string "," <* skipSpaces)

connection :: Parser Reference
connection =
  choice
    [ try parameterRef
    , NodeRef <$> identifier
    ]

parameterRef :: Parser Reference
parameterRef =
  ParameterRef <$> identifier <*> ((string ".") *> identifier)

-- scalar params

-- a general Number attribute
numberAttribute :: String -> Parser (Tuple String AudioAttribute)
numberAttribute paramName =
  Tuple <$> keyWord paramName <*> (numberAttr <$> number)
    <?> paramName

-- audio params

-- a general audio parameter attribute
audioParamAttribute :: String -> Parser (Tuple String AudioAttribute)
audioParamAttribute paramName =
  Tuple <$> keyWord paramName <*> audioParams
    <?> paramName

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
    (openCurlyBracket *> (audioParamAttribute "gain") <* closeCurlyBracket)

-- oscillator attributes

oscillatorAttributes :: Parser AttributeMap
oscillatorAttributes =
  fromFoldable <$>
    (openCurlyBracket *> oscillatorAttributeList <* closeCurlyBracket)

oscillatorAttributeList :: Parser (List (Tuple String AudioAttribute))
oscillatorAttributeList =
  sepBy
    (choice
      [
        oscillatorTypeAttribute
      , audioParamAttribute "frequency"
      , audioParamAttribute "detune"
      ]
    ) comma

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

audioBufferSourceAttributes:: Parser AttributeMap
audioBufferSourceAttributes =
  fromFoldable <$>
    (openCurlyBracket *> audioBufferSourceAttributeList <* closeCurlyBracket)

audioBufferSourceAttributeList :: Parser (List (Tuple String AudioAttribute))
audioBufferSourceAttributeList =
  sepBy
    (choice
      [
        urlAttribute
      , loopAttribute
      , setLoopStartAttribute
      , setLoopEndAttribute
      ]
    ) comma

urlAttribute :: Parser (Tuple String AudioAttribute)
urlAttribute =
  Tuple <$> keyWord "url" <*> urlStringAttribute

setLoopStartAttribute :: Parser (Tuple String AudioAttribute)
setLoopStartAttribute =
  numberAttribute "setLoopStart"

setLoopEndAttribute :: Parser (Tuple String AudioAttribute)
setLoopEndAttribute =
  numberAttribute "setLoopEnd"

loopAttribute :: Parser (Tuple String AudioAttribute)
loopAttribute =
  Tuple <$> keyWord "loop" <*> boolAttribute

biquadFilterAttributes :: Parser AttributeMap
biquadFilterAttributes =
  fromFoldable <$>
    (openCurlyBracket *> biquadFilterAttributeList <* closeCurlyBracket)

biquadFilterAttributeList :: Parser (List (Tuple String AudioAttribute))
biquadFilterAttributeList =
  sepBy
    (choice
      [
        biquadFilterTypeAttribute
      , audioParamAttribute  "frequency"
      , audioParamAttribute "quality"
      ]
    ) comma

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

-- at the moment we require a delayTime attribute, nothing more
delayAttributes :: Parser AttributeMap
delayAttributes =
  (fromFoldable <<< L.singleton) <$>
    (openCurlyBracket *> (audioParamAttribute "delayTime") <* closeCurlyBracket)

-- at the moment we require a pan attribute, nothing more
stereoPannerAttributes :: Parser AttributeMap
stereoPannerAttributes =
  (fromFoldable <<< L.singleton) <$>
    (openCurlyBracket *> (audioParamAttribute "pan") <* closeCurlyBracket)

-- dynamics compressor attributes

dynamicsCompressorAttributes :: Parser AttributeMap
dynamicsCompressorAttributes  =
  fromFoldable <$>
    (openCurlyBracket *> dynamicsCompressorAttributeList <* closeCurlyBracket)

dynamicsCompressorAttributeList :: Parser (List (Tuple String AudioAttribute))
dynamicsCompressorAttributeList =
  sepBy
    (choice
      [
        audioParamAttribute "threshold"
      , audioParamAttribute "knee"
      , audioParamAttribute "ratio"
      , audioParamAttribute "attack"
      , audioParamAttribute "release"
      ]
    ) comma

convolverAttributes:: Parser AttributeMap
convolverAttributes =
  fromFoldable <$>
    (openCurlyBracket *> convolverAttributeList <* closeCurlyBracket)

convolverAttributeList :: Parser (List (Tuple String AudioAttribute))
convolverAttributeList =
  sepBy
    (choice
      [
        urlAttribute
      , normalizeAttribute
      ]
    ) comma

normalizeAttribute :: Parser (Tuple String AudioAttribute)
normalizeAttribute =
  Tuple <$> keyWord "normalize" <*> boolAttribute

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
    number) <*> time
    <?> "setValueAtTime"

linearRampToValueAtTime :: Parser AudioParamDef
linearRampToValueAtTime  =
  LinearRampToValueAtTime <$> ((keyWord "linearRampToValueAtTime") *>
    number) <*> time
    <?> "linearRampToValueAtTime"

exponentialRampToValueAtTime :: Parser AudioParamDef
exponentialRampToValueAtTime  =
  ExponentialRampToValueAtTime  <$> ((keyWord "exponentialRampToValueAtTime") *>
    number) <*> time
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

time :: Parser Time
time =
  relativeTime <|> absoluteTime

-- absolute time is javascript time
absoluteTime :: Parser Time
absoluteTime  =
  Absolute <$> number

-- relative tome is relative to audio context 'now'
relativeTime :: Parser Time
relativeTime =
  Relative <$> ((string "t" <* skipSpaces <* string "+" <* skipSpaces) *> number)

intAttribute :: Parser AudioAttribute
intAttribute =
  (numberAttr <<< toNumber) <$> unsignedInt <* skipSpaces

boolAttribute :: Parser AudioAttribute
boolAttribute =
  boolAttr <$> (isTrue <|> isFalse)

isTrue :: Parser Boolean
isTrue =
  pure true <* string "true" <* skipSpaces

isFalse :: Parser Boolean
isFalse =
  pure false <* string "false" <* skipSpaces

-- attempt a lax validation of URLS - just ban illegal characters
-- we can't allow a comma in a URL because it's used as a separator
urlStringAttribute :: Parser AudioAttribute
urlStringAttribute =
  let
    pattern = "[A-Za-z0-9\\/\\.\\+\\?\\[\\]\\{\\/\\*\\+;=`_~:@!&'#-]*"
    --  pattern = "[A-Za-z0-9///./?/[/]/$/(/)/*/+,;=`_~:@!&'#-]*"
  in
    stringAttr <$> regex pattern <* skipSpaces
      <?> "url string"

-- symbol table operations

-- check that an identifier for a new node has not already been used
-- if valid, then add to the symbol table
checkValidNodeId :: SymbolTable -> String -> Parser (Tuple String SymbolTable)
checkValidNodeId st nodeName =
  if (nodeName == "output") then
    fail ("identifier: output is reserved as the default output node")
  else if Set.member nodeName st.nodeNames then
    fail ("identifier: " <> nodeName <> " has already been used")
  else
    let
      nodeNames = Set.insert nodeName st.nodeNames
    in
      pure (Tuple nodeName {nodeNames} )

-- builders

buildNode :: NodeType -> Tuple String SymbolTable -> AttributeMap -> Set.Set Reference -> Tuple NodeDef SymbolTable
buildNode nodeType (Tuple id st) attributes connections =
  Tuple (NodeDef{ nodeType, id, attributes, connections} ) st

buildNodeList :: Tuple NodeDef SymbolTable -> Tuple (List NodeDef) SymbolTable -> Tuple AudioGraph SymbolTable
buildNodeList (Tuple n _) (Tuple ns st) =
  Tuple (n : ns) st

-- | a parse error and its accompanying position in the text
newtype PositionedParseError = PositionedParseError
  { pos :: Int
  , error :: String
  }

instance showKeyPositionedParseError :: Show PositionedParseError where
  show (PositionedParseError err) = err.error <> " at position " <> show err.pos

-- | Run a parser for an input string, returning either a positioned error or a result.
runParser1 :: forall a. Parser a -> String -> Either PositionedParseError a
runParser1 (Parser p) s =
  let
    formatErr :: { pos :: Pos, error :: ParseError } -> PositionedParseError
    formatErr { pos : pos, error : ParseError err } =
      PositionedParseError { pos : pos, error : err}
  in
    bimap formatErr _.result (p { str: s, pos: 0 })

-- | Parse an audio graph
parse :: String -> Either PositionedParseError (Tuple AudioGraph SymbolTable)
parse s =
  runParser1 (audioNodes initialSymbolTable) s
