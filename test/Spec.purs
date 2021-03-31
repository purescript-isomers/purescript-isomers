module Test.Spec where

import Prelude

import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (RequestBuildersStep(..), ResponseM(..), requestBuilders, runResponseM)
import Isomers.Client (client) as Client
import Isomers.Node.Server (serve) as Node.Server
import Isomers.Request (Duplex(..), Duplex', print) as Request
import Isomers.Request.Duplex.Parser (int) as Parser
import Isomers.Request.Duplex.Parser (int) as Request.Duplex
import Isomers.Request.Duplex.Record (Root, empty, intSegment, segment) as Request.Duplex.Record
import Isomers.Response (Duplex(..), Duplex') as Response
import Isomers.Response.Duplex (asJson) as Response.Duplex
import Isomers.Server (router) as Server
import Isomers.Spec (Spec(..), root, spec) as Spec
import Isomers.Spec (spec) as Spec.Builder
import Node.Stream (onClose)
import Polyform.Batteries.Json.Duals ((:=))
import Polyform.Batteries.Json.Duals (Pure, int, object, string) as Json.Duals
import Polyform.Dual (Dual(..))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Validator.Dual (runSerializer, runValidator)
import Polyform.Validator.Dual.Pure (runSerializer, runValidator) as Dual.Pure
import Prim.Row (class Lacks) as Row
import Type.Prelude (Proxy(..), SProxy(..))

responseDuplex = Response.Duplex.asJson (Dual.Pure.runSerializer d) (lmap unsafeStringify <<< un V <<< Dual.Pure.runValidator d)
  where
  d ∷ Json.Duals.Pure _ { a ∷ Int, b ∷ String }
  d = Json.Duals.object >>> rec
    where
    rec =
      Dual.Record.build
        $ (SProxy ∷ SProxy "a")
        := Json.Duals.int
        <<< (SProxy ∷ SProxy "b")
        := Json.Duals.string

requestDuplex ::
  ∀ reqBody res.
  Request.Duplex.Record.Root reqBody ( productId ∷ Int )
requestDuplex = Request.Duplex.Record.intSegment (SProxy ∷ SProxy "productId")

api =
  Spec.root
    { shop: requestDuplex /\ responseDuplex
    , admin: Request.Duplex.Record.empty /\ (pure unit ∷ Response.Duplex' Unit)
    , sub:
        { shop: requestDuplex /\ responseDuplex }
    }

client = do
  let
    Spec.Spec { request: reqDpl, response: resDpls } = api
  Client.client reqDpl resDpls

handlers =
  { shop: const $ pure { a: 8, b: "test" }
  , admin: const $ pure unit
  , sub:
      { shop: \r → pure { a: r.productId, b: "sub-test" } }
  }

router = do
  Server.router
    api
    { shop: const $ pure { a: 8, b: "test" }
    , admin: const $ pure unit
    , sub:
        { shop: const $ pure { a: 8, b: "test" } }
    }

z ::
  forall t419.
  ResponseM t419
    { a :: Int
    , b :: String
    }
z = client.shop { productId: 8 }

specRequestBuilder ∷
  ∀ body input request requestBuilders response.
  HFoldlWithIndex (RequestBuildersStep request request) {} (Proxy request) { | requestBuilders } ⇒
  Spec.Spec body input request response → { | requestBuilders }
specRequestBuilder (Spec.Spec _) = requestBuilders (Proxy ∷ Proxy request)

print (Spec.Spec { request: reqDpl }) = Request.print reqDpl

req = specRequestBuilder api

main :: Effect Unit
main = do
  onClose ← Node.Server.serve api handlers { hostname: "127.0.0.1", port: 9000, backlog: Nothing } (log "127.0.0.1:9000")
  onClose (log "Closed")

  log $ _.path <<< print api $ req.admin {}
  log $ _.path <<< print api $ req.sub.shop {productId: 8}

  launchAff_ $ runResponseM z
