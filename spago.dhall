{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "audiograph"
, dependencies =
  [ "aff"
  , "affjax"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "http-methods"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "string-parsers"
  , "tuples"
  , "variant"
  , "webaudio"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
