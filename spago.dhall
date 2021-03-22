{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arraybuffer-types"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "heterogeneous"
  , "node-http"
  , "node-streams"
  , "polyform"
  , "polyform-batteries-core"
  , "polyform-batteries-json"
  , "psci-support"
  , "record-extra"
  , "record-prefix"
  , "request-duplex-variant"
  , "routing-duplex"
  , "routing"
  , "run"
  , "stringutils"
  , "tuples"
  , "typelevel-eval"
  , "web-fetch"
  , "wire-react"
  , "wire-react-router"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
