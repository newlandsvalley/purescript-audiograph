let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "simple-examples/**/*.purs" ],
  dependencies = conf.dependencies # [ "console"
                                     , "datetime"
                                     , "effect"
                                     ],
  packages = ./editor-packages.dhall
}
