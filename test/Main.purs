module Test.Main where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Number (fromString) as Number
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Routing.Duplex (int, segment) as Routing.Duplex
import Routing.Duplex.Parser (RouteError)
import Run (Run)
import Run.Except (EXCEPT)
import Type.Prelude (SProxy(..))
import WebRow.Hybrid.Contrib.Routing.Duplex (unitDuplex)
import WebRow.Hybrid.Router (Router)
import WebRow.Hybrid.Router (duplex, prefix, router) as Router
import WebRow.Hybrid.Router.Run (FETCH, Request, Response(..), ResponseCodec(..))
import WebRow.Hybrid.Router.Run (route) as Router.Run

number ∷ ResponseCodec Number
number =
  ResponseCodec
    { decode: Number.fromString <=< (_.content <<< un Response)
    , encode: \content → Response { statusCode: 200, content: Just (show content) }
    }

string ∷ ResponseCodec String
string =
  ResponseCodec
    { decode: _.content <<< un Response
    , encode: \content → Response { statusCode: 200, content: Just content }
    }

-- | Type signature is fully derived and optional here.
router ::
  Router
    ( "admin.dashboard" ∷ ResponseCodec Number
    , "admin.profile" ∷ ResponseCodec String
    )
    ( "admin.dashboard" ∷ Tuple Int Unit
    , "admin.profile" ∷ Tuple Int Int
    )
router =
  Router.duplex (Routing.Duplex.int Routing.Duplex.segment)
    $ Router.prefix (SProxy ∷ SProxy ".")
        { admin:
            Router.router true
              { dashboard: unitDuplex Routing.Duplex.segment /\ number
              , profile: Routing.Duplex.int Routing.Duplex.segment /\ string
              }
        }

route ::
  ∀ eff.
  Either (Request String)
    ( Variant
        ( "admin.dashboard" ∷ Int /\ Unit
        , "admin.profile" ∷ Int /\ Int
        )
    ) ->
  Run
    ( "admin.dashboard" ∷ FETCH (Int /\ Unit) Number
    , "admin.profile" ∷ FETCH (Int /\ Int) String
    , routeNotFound ∷ EXCEPT (RouteError /\ Request String)
    | eff
    )
    String
route =
  Router.Run.route router
    { "admin.dashboard": const $ pure "TEST"
    , "admin.profile": const $ pure "TEST"
    }
