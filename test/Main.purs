module Test.Main where

import Prelude
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console (log)
import Hybrid.Api.Spec (ResponseCodec(..))
import Hybrid.App.Server (route) as App.Server
import Hybrid.App.Spec (Raw(..))
import Hybrid.App.Spec (duplex, prefix, spec) as App.Spec
import Hybrid.Contrib.Request.Duplex (unitDuplex)
import Hybrid.Response (Response(..))
import Request.Duplex (Request)
import Request.Duplex (Request, int, print, segment) as Request.Duplex
import Request.Duplex.Parser (RouteError)
import Request.Duplex.Types (Method(..))
import Run (Run)
import Run.Except (EXCEPT)
import Type.Prelude (SProxy(..))

number ∷ ResponseCodec Number
number =
  ResponseCodec
    { decode: Number.fromString
    , encode: show
    }

string ∷ ResponseCodec String
string =
  ResponseCodec
    { decode: Just
    , encode: identity
    }

-- router ::
--   Router
--     ( "admin.dashboard" ∷ ResponseCodec Number
--     , "admin.profile" ∷ ResponseCodec String
--     )
--     ( "admin.dashboard" ∷ Tuple Int Unit
--     , "admin.profile" ∷ Tuple Int Int
--     )
spec =
  App.Spec.duplex (Request.Duplex.int Request.Duplex.segment)
    $ App.Spec.prefix
        (SProxy ∷ SProxy ".")
        { admin:
            App.Spec.spec true
              { dashboard: unitDuplex Request.Duplex.segment /\ number /\ \req res → "TEST"
              , profile: Request.Duplex.int Request.Duplex.segment /\ string /\ \req res → "TEST"
              }
        }

-- route = App.Server.route spec
main ∷ Effect Unit
main = do
  let
    Raw raw = spec
  traceM $ Request.Duplex.print raw.codecs.request (Variant.inj (SProxy ∷ SProxy "admin.profile") $ 9 /\ 8)
  traceM $ Request.Duplex.print raw.codecs.request (Variant.inj (SProxy ∷ SProxy "admin.dashboard") $ 9 /\ unit)
