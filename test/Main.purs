module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Hybrid.Api.Spec (ResponseCodec(..))
import Hybrid.App.Spec (Raw(..))
import Hybrid.App.Spec (duplex, endpoints, prefix, prefixLabels) as App.Spec
import Hybrid.Contrib.Request.Duplex (unitDuplex)
import Hybrid.HTTP.Request.Method (get) as Method
import Request.Duplex (int, segment) as Request.Duplex
import Request.Duplex.Generic.Variant (methodVariant)
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

-- spec' =
--   -- App.Spec.prefixLabels
--   --   (SProxy ∷ SProxy ".")
--     { test:
--         { admin:
--           { "dashboard"
--             Method $ Method.get
--               ( App.Spec.endpoints true
--                 { dashboard: unitDuplex /\ number /\ \_ req → "TEST"
--                 , profile: Request.Duplex.int Request.Duplex.segment /\ string /\ \_ req → "TEST"
--                 }
--               )
--           , "POST": App.Spec.endpoints true
--             { dashboard: unitDuplex /\ number /\ \_ req → "TEST"
--             , profile: Request.Duplex.int Request.Duplex.segment /\ string /\ \_ req → "TEST"
--             }
--           }
--         }
--     }

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
    $ App.Spec.prefixLabels
        (SProxy ∷ SProxy ".")
        { test:
            { admin:
                App.Spec.endpoints true
                  { dashboard: unitDuplex /\ number /\ \_ req → "TEST"
                  , profile: Request.Duplex.int Request.Duplex.segment /\ string /\ \_ req → "TEST"
                  }
            }
        }

-- route = App.Server.route spec
main ∷ Effect Unit
main = do
  let
    Raw raw = spec
  pure unit

-- traceM $ Request.Duplex.print raw.codecs.request (Variant.inj (SProxy ∷ SProxy "admin.profile") $ 9 /\ 8)
-- traceM $ Request.Duplex.print raw.codecs.request (Variant.inj (SProxy ∷ SProxy "admin.dashboard") $ 9 /\ unit)
