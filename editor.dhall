let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "audiograph-editor/**/*.purs" ],
  dependencies = conf.dependencies # [ "arrays"
                                     , "colors"
                                     , "css"
                                     , "dom-indexed"
                                     , "effect"
                                     , "halogen"
                                     , "halogen-css"
                                     , "halogen-components"
                                     , "js-fileio"
                                     , "media-types"
                                     , "random"
                                     , "strings"
                                     ],
  packages = ./editor-packages.dhall
}
