{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "audiograph"
, dependencies =
  [ "affjax"
  , "console"
  , "effect"
  , "ordered-collections"
  , "prelude"
  , "string-parsers"
  , "variant"
  , "webaudio"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
