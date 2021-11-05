let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210506/packages.dhall sha256:d199e142515f9cc15838d8e6d724a98cd0ca776ceb426b7b36e841311643e3ef

in  upstream
  with
    homogeneous = mkPackage
      [ "assert", "console", "effect", "foreign-object", "psci-support"
      , "record-extra", "typelevel-eval", "variant"
      ]
      "https://github.com/paluh/purescript-homogeneous.git"
      "v0.3.0"
  with
    http-types =
      { dependencies = [ "tuples", "unicode"]
      , repo = "https://github.com/Woody88/purescript-http-types.git"
      , version = "3bcbf8b63d6ffce66356186519365cbf38697eb2"
      }
  with
    js-unsafe-stringify =
      { dependencies = ([] : List Text)
      , repo = "https://github.com/paluh/purescript-js-unsafe-stringify"
      , version = "03cbaf1837c85ad06402226a2f564a384ab8a96e"
      }
  with
    polyform = ../polyform/spago.dhall as Location
  with
    polyform-batteries-core = ../batteries-core/spago.dhall as Location
  with
    polyform-batteries-json = ../batteries-json/spago.dhall as Location
  -- with
  --   request-duplex = ../request-duplex/spago.dhall as Location
  -- with
  --   request-duplex-variant = ../request-duplex-variant/spago.dhall as Location
  with
    record-prefix = ../../external/purescript-record-prefix/spago.dhall as Location
    -- record-prefix = mkPackage
    --   [ "avar", "console", "prelude", "react-basic-hooks" ]
    --   "https://github.com/paluh/purescript-record-prefix"
    --   "master"
  with
    -- typelevel-eval = mkPackage
    --   [ "console", "effect", "leibniz", "psci-support", "record" ]
    --   "https://github.com/natefaubion/purescript-typelevel-eval.git"
    --   "04e86ce3be5c46a7a13270d4fca183af6de648f5"
    typelevel-eval = mkPackage
      [ "console", "effect", "leibniz", "psci-support", "record" ]
      "https://github.com/mikesol/purescript-typelevel-eval"
      "mikesol"
  with
    web-fetch = ../purescript-web-fetch/spago.dhall as Location
    --   { dependencies =
    --     [ "effect"
    --     , "foreign-object"
    --     , "http-methods"
    --     , "prelude"
    --     , "record"
    --     , "typelevel-prelude"
    --     , "web-file"
    --     , "web-promise"
    --     , "web-streams"
    --     ]
    --   , repo = "https://github.com/purescript-web/purescript-web-fetch.git"
    --   , version = "v1.0.1"
    --   }
  with
    web-file =
    { dependencies = [ "foreign", "media-types", "web-dom" ]
    , repo = "https://github.com/purescript-web/purescript-web-file.git"
    , version = "v2.3.0"
    }
  with
    webrow = mkPackage
      [ "b64", "console", "crypto", "effect", "homogeneous", "httpure", "logging-journald"
      , "optparse", "polyform-batteries-env", "postgresql-client", "profunctor-lenses"
      , "psci-support", "record", "record-extra", "routing-duplex-variant", "resourcet", "run"
      , "run-streaming", "selda", "simple-jwt", "smolder", "spec", "strings"
      , "string-parsers", "typelevel-eval", "undefined-is-not-a-problem", "uuid"
      ]
      "https://github.com/purescript-webrow/webrow.git"
      "b6b928ede6ed5430f759bb39f268061377d5ccd9"
  with
    web-promise =
      { dependencies =
        [ "effect"
        , "foldable-traversable"
        , "exceptions"
        , "functions"
        , "maybe"
        , "prelude"
        ]
      , repo = "https://github.com/purescript-web/purescript-web-promise.git"
      , version = "v1.0.3"
      }
  with
    wire-react = mkPackage
      [ "wire", "free", "freet", "react-basic-hooks" ]
      "https://github.com/robertdp/purescript-wire-react"
      "v0.0.1"
  with
    web-router = mkPackage
      ["aff", "effect", "freet", "indexed-monad", "prelude", "profunctor-lenses", "routing" ]
      "https://github.com/robertdp/purescript-web-router"
      "v0.3.0"
  -- with
  --   wire-react-router = mkPackage
  --     [ "aff"
  --     , "freet"
  --     , "indexed-monad"
  --     , "profunctor-lenses"
  --     , "react-basic-hooks"
  --     , "routing"
  --     ]
  --     "https://github.com/robertdp/purescript-web-router.git"
  --     "v0.2.1"
  with
    web-streams =
      { dependencies =
        [ "arraybuffer-types"
        , "effect"
        , "exceptions"
        , "nullable"
        , "prelude"
        , "tuples"
        , "web-promise"
        ]
      , repo = "https://github.com/purescript-web/purescript-web-streams.git"
      , version = "v1.0.0"
      }
  -- wire-react-router = mkPackage
  --   [ "wire", "free", "freet", "react-basic-hooks" ]
  --   "https://github.com/paluh/purescript-wire-react-router"
  --   "master"
