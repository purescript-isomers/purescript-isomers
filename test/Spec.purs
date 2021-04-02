module Test.Spec where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (RequestBuildersStep(..), ResponseM(..), requestBuilders, runResponseM)
import Isomers.Client (client) as Client
import Isomers.Contrib.Heterogeneous.List (HNil(..), (:))
import Isomers.HTTP.ContentTypes (TextMime, _json)
import Isomers.HTTP.Request (Method(..))
import Isomers.Node.Server (serve) as Node.Server
import Isomers.Request (Duplex(..), Duplex', print) as Request
import Isomers.Request.Duplex.Parser (int) as Parser
import Isomers.Request.Duplex.Record (Root, empty, intSegment, segment) as Request.Duplex.Record
import Isomers.Response (Okayish, OkayishDuplex') as Response
import Isomers.Response.Duplex (header, withHeaderValue) as Response.Duplex
import Isomers.Response.Okayish.Duplexes (asJson) as Response.Okayish.Duplexes
import Isomers.Response.Okayish.Type (fromEither) as Response.Okayish
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
import Type.Prelude (Proxy(..), SProxy(..))

responseDuplex = Response.Duplex.withHeaderValue (CaseInsensitiveString "X-TEST") "TEST value" $ Response.Okayish.Duplexes.asJson (Dual.Pure.runSerializer d) (lmap unsafeStringify <<< un V <<< Dual.Pure.runValidator d)
  where
  d ∷ Json.Duals.Pure _ { a ∷ Int, b ∷ String, method ∷ String }
  d = Json.Duals.object >>> rec
    where
    rec =
      Dual.Record.build
        $ (SProxy ∷ SProxy "a")
        := Json.Duals.int
        <<< (SProxy ∷ SProxy "b")
        := Json.Duals.string
        <<< (SProxy ∷ SProxy "method")
        := Json.Duals.string

requestDuplex ::
  ∀ reqBody res.
  Request.Duplex.Record.Root reqBody ( productId ∷ Int )
requestDuplex = Request.Duplex.Record.intSegment (SProxy ∷ SProxy "productId")

api =
  Spec.root
    { shop: requestDuplex /\ (responseDuplex : HNil)
    , admin: Request.Duplex.Record.empty /\ ((pure $ pure unit) ∷ Response.OkayishDuplex' TextMime () Unit)
    , sub:
        { shop: Method
          { "GET": requestDuplex /\ responseDuplex
          , "POST": requestDuplex /\ responseDuplex
          }
        }
    }

client = do
  let
    Spec.Spec { request: reqDpl, response: resDpls } = api
  Client.client reqDpl resDpls

handlers =
  { shop: { "application/json": const $ pure $ Response.Okayish.fromEither $ Right { a: 8, b: "test", method: "ANY" }}
  , admin: const $ pure (pure unit)
  , sub:
    { shop:
      { "GET": \r → pure $ Response.Okayish.fromEither $ Right { a: r.productId, b: "sub-test", method: "received GET" }
      , "POST": \r → pure $ Response.Okayish.fromEither $ Right { a: r.productId, b: "sub-test", method: "received POST" }
      }
    }
  }

router = do
  Server.router
    api
    handlers

z = client.shop."application/json" { productId: 8 }

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
  log $ _.path <<< print api $ req.sub.shop."GET" {productId: 8}
  log $ unsafeStringify <<< print api $ req.shop."application/json" { productId: 8 }

  -- launchAff_ $ runResponseM z
