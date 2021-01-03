let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "simple-examples/**/*.purs" ],
  dependencies = conf.dependencies # [ "console"
                                     , "effect"
                                     ],
  packages = ./editor-packages.dhall
}
