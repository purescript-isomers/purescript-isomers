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
  , "heterogeneous"
  , "polyform"
  , "polyform-batteries-core"
  , "polyform-batteries-json"
  , "psci-support"
  , "record-extra"
  , "record-prefix"
  , "routing-duplex-variant"
  , "run"
  , "tuples"
  , "typelevel-eval"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
