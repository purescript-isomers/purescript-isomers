{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "effect"
  , "foreign-object"
  , "heterogeneous"
  , "milkis"
  , "node-http"
  , "polyform"
  , "polyform-batteries-core"
  , "polyform-batteries-json"
  , "psci-support"
  , "record-extra"
  , "record-prefix"
  , "request-duplex-variant"
  , "routing"
  , "run"
  , "stringutils"
  , "tuples"
  , "typelevel-eval"
  , "wire-react"
  , "wire-react-router"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
