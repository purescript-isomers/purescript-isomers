module Test.Api where

import Prelude

import Control.Monad.Except (class MonadError)
import Data.Functor.Variant (FProxy(..), VariantF)
import Effect.Aff (Aff)
import Heterogeneous.Folding (hfoldl)
import Isomers.Contrib.Heterogeneous.List (HNil(..), (:))
import Isomers.HTTP (Response(..))
import Isomers.HTTP.Headers.Accept (ResponseFolding(..)) as Accept
import Isomers.HTTP.Headers.Accept (requestFolding)
import Isomers.HTTP.Response (Duplex(..), Duplex', fromJsonDual) as Response
import Isomers.HTTP.Response (Ok, Response', notFound)
import Isomers.HTTP.Response (OkF(..), redirect)
import Isomers.HTTP.Response.Duplex (Duplex(..))
import Network.HTTP.Types (hContentType)
import Polyform.Batteries.Json.Duals (int) as Json.Duals
import Request.Duplex (header) as RequestDuplex
import Type.Row (type (+))

-- import Prelude
-- import Data.Maybe (Maybe(..))
-- import Data.Number (fromString) as Number
-- import Data.Tuple.Nested ((/\))
-- import Effect (Effect)
-- import Isomers.Api.Spec (ResponseCodec(..))
-- import Isomers.App.Spec (Raw(..))
-- import Isomers.App.Spec (duplex, endpoints, prefixLabels) as App.Spec
-- import Isomers.Contrib.Request.Duplex (unitDuplex)
-- import Request.Duplex (int, segment) as Request.Duplex
-- import Type.Prelude (SProxy(..))
-- 
-- number ∷ ResponseCodec Number
-- number =
--   ResponseCodec
--     { decode: Number.fromString
--     , encode: show
--     }
-- 
-- string ∷ ResponseCodec String
-- string =
--   ResponseCodec
--     { decode: Just
--     , encode: identity
--     }
-- 
-- 
-- spec = App.Spec.duplex (Request.Duplex.int Request.Duplex.segment)
--    $ App.Spec.prefixLabels
--        (SProxy ∷ SProxy ".")
--        { test:
--            { admin:
--                App.Spec.endpoints true
--                  { dashboard: unitDuplex /\ number /\ \_ req → "TEST"
--                  , profile: Request.Duplex.int Request.Duplex.segment /\ string /\ \_ req → "TEST"
--                  }
--            }
--        }
-- 
-- -- route = App.Server.route spec
-- main ∷ Effect Unit
-- main = do
--   let
--     Raw raw = spec
--   pure unit
-- 
-- -- traceM $ Request.Duplex.print raw.codecs.request (Variant.inj (SProxy ∷ SProxy "admin.profile") $ 9 /\ 8)
-- -- traceM $ Request.Duplex.print raw.codecs.request (Variant.inj (SProxy ∷ SProxy "admin.dashboard") $ 9 /\ unit)

-- i :: forall t3.
--    Response.Duplex Aff
--                  (VariantF
--                     ( ok :: FProxy (OkF "application/json")
--                     )
--                     Int
--                  )
--                  (VariantF
--                     ( ok :: FProxy (OkF "application/json")
--                     )
--                     Int
--                  )
i = Response.fromJsonDual Json.Duals.int

-- x :: forall t13.
--    Monad t13 => HCons
--                   (Router
--                      (Duplex t13
--                         (VariantF
--                            ( notFound :: FProxy NotFoundF
--                            , ok :: FProxy (OkF "application/json")
--                            )
--                            Int
--                         )
--                         (VariantF
--                            ( notFound :: FProxy NotFoundF
--                            , ok :: FProxy (OkF "application/json")
--                            )
--                            Int
--                         )
--                      )
--                      Transitioning
--                      Resolved
--                      Unit
--                   )
--                   HNil
x = (((notFound (redirect i))) : HNil)

s = hfoldl Accept.ResponseFolding {} (redirect (notFound i) : HNil)

t = requestFolding (RequestDuplex.header hContentType) (((notFound (redirect i))) : HNil)
