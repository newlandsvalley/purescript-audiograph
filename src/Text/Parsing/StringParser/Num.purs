module Text.Parsing.StringParser.Num (int, unsignedInt, number, numberOrInt, sign) where

-- | Possible useful eventual additions to StringParser
-- | to enable parsing of numeric strings

import Prelude ((<<<), (<$>), (<$), (*), (<*>), negate)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust, fromMaybe)
import Data.Int (fromString, toNumber)
import Data.Number (fromString) as Num
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
    <*> (readInt <$> regex "(0|[1-9][0-9]*)")
    <?> "expected an integer"

-- | Parse an unsigned integer.
unsignedInt :: Parser Int
unsignedInt =
  (readInt <$> regex "(0|[1-9][0-9]*)")
    <?> "expected an unsigned integer"


-- | Parse a number
number :: Parser Number
number =
  (*)
    <$> toNumber <$> sign
    <*> (readNumber <$> regex "(0|[1-9][0-9]*)(\\.[0-9]+)")
    <?> "expected a number"

-- | Parse a number which may or may not have a decimal point
-- | we need to try number because both will consume any sign
numberOrInt :: Parser Number
numberOrInt =
  try number <|> (toNumber <$> int)
  <?> "expected a number (with or without decimal point)"


readInt :: String -> Int
readInt s =
  unsafePartial (fromJust <<< fromString) s


readNumber :: String -> Number
readNumber s =
  unsafePartial (fromJust <<< Num.fromString) s



