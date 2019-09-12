module Text.Parsing.StringParser.Num (int, unsignedInt, number, numberOrInt, sign) where

-- | Possible useful eventual additions to StringParser
-- | to enable parsing of numeric strings

import Prelude ((<<<), (<$>), (<$), (*), (<*>), negate)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust, fromMaybe)
import Data.Int (fromString, toNumber)
import Global (readFloat)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (choice, optionMaybe, (<?>))
import Text.Parsing.StringParser.CodePoints (string, regex)

-- | Parse a numeric sign, returning `1` for positive numbers and `-1`
-- for negative numbers.
sign :: Parser Int
sign =
  fromMaybe 1 <$>
    optionMaybe (choice [  1 <$ string "+"
                          , -1 <$ string "-" ])

-- | Parse an integer.
int :: Parser Int
int =
  (*)
    <$> sign
    <*> (toInt <$> regex "(0|[1-9][0-9]*)")
    <?> "expected an integer"

-- | Parse an unsigned integer.
unsignedInt :: Parser Int
unsignedInt =
  (toInt <$> regex "(0|[1-9][0-9]*)")
    <?> "expected an unsigned integer"


-- | Parse a number
number :: Parser Number
number =
  (*)
    <$> toNumber <$> sign
    <*> (readFloat <$> regex "(0|[1-9][0-9]*)(\\.[0-9]+)")
    <?> "expected a number"

-- | Parse a number which may or may not have a decimal point
-- | we need to try number because both will consume any sign
numberOrInt :: Parser Number
numberOrInt =
  try number <|> (toNumber <$> int)
  <?> "expected a number (with or without decimal point)"


toInt :: String -> Int
toInt s =
  unsafePartial (fromJust <<< fromString) s
