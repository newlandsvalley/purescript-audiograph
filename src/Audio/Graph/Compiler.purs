module Audio.Graph.Compiler (compile) where

import Data.Tuple (fst)
import Data.Either (Either(..))
import Audio.Graph.Parser (parse)
import Audio.Graph (AudioGraph)
import Prelude (($), show)

-- | A compiler which perfoms both parsing and semantic checks
compile :: String -> Either String AudioGraph
compile s =
  case parse s of
    Right n ->
      Right (fst n)

    Left e ->
      Left $ show e
