let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "audiograph-editor/**/*.purs" ],
  dependencies = conf.dependencies # [ "console"
                                     , "effect"
                                     , "halogen"
                                     , "halogen-css"
                                     , "halogen-components"
                                     ],
  packages = ./editor-packages.dhall
}
