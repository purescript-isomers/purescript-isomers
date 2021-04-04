module Test.Web where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V(..))
import Data.Variant (case_, default, on) as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (random)
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (RequestBuildersStep(..), ResponseM(..), requestBuilders, runResponseM)
import Isomers.Client (client) as Client
import Isomers.Contrib.Heterogeneous.List (HNil(..), (:))
import Isomers.HTTP (Exchange(..))
import Isomers.HTTP.ContentTypes (_json)
import Isomers.HTTP.Request (Method(..))
import Isomers.Node.Server (serve) as Node.Server
import Isomers.Request (Duplex(..), Duplex', print) as Request
import Isomers.Request.Duplex.Parser (int) as Parser
import Isomers.Request.Duplex.Record (Root, empty, intSegment, segment) as Request.Duplex.Record
import Isomers.Response (Duplex(..), Duplex') as Response
import Isomers.Response (Okayish)
import Isomers.Response.Duplex (asJson) as Response.Duplex
import Isomers.Response.Okayish (fromEither, toVariant) as Okayish
import Isomers.Response.Okayish.Duplexes (asJson) as Response.Okayish.Duplexes
import Isomers.Response.Okayish.Type (_ok)
import Isomers.Response.Okayish.Type (fromEither) as Okayish.Type
import Isomers.Response.Raw (RawServer(..))
import Isomers.Response.Types (HtmlString(..))
import Isomers.Server (router) as Server
import Isomers.Spec (Spec(..)) as Spec
import Isomers.Web.Server (renderToApi)
import Isomers.Web.Spec (root)
import Isomers.Web.Spec.Builder (Rendered(..))
import Isomers.Web.Spec.Builder (spec) as Web.Builder
import Isomers.Web.Spec.Builder (spec) as Web.Spec.Builder
import Isomers.Web.Spec.Type (Spec(..))
import Network.HTTP.Types (internalServerError500, ok200)
import Node.Stream (onClose)
import Polyform.Batteries.Json.Duals ((:=))
import Polyform.Batteries.Json.Duals (Pure, arrayOf, int, object, string) as Json.Duals
import Polyform.Dual (Dual(..))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Validator.Dual (runSerializer, runValidator)
import Polyform.Validator.Dual.Pure (runSerializer, runValidator) as Dual.Pure
import Test.Spec (specRequestBuilder)
import Test.X (render) as X
import Type.Prelude (Proxy(..), SProxy(..))

responseDuplex = responseDual d
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

responseDual d = Response.Okayish.Duplexes.asJson (Dual.Pure.runSerializer d) (lmap unsafeStringify <<< un V <<< Dual.Pure.runValidator d)

requestDuplex ::
  ∀ reqBody res.
  Request.Duplex.Record.Root reqBody ( productId ∷ Int )
requestDuplex = Request.Duplex.Record.intSegment (SProxy ∷ SProxy "productId")

newtype HTML
  = HTML String

get view = Method { "GET": view }

htmlResponse (Exchange _ (Just (Right res))) =
  ( Variant.on _ok
    (\r → RawServer
      { status: ok200
      , headers: mempty
      , body: HtmlString $ "<h1>" <> unsafeStringify r <> "</h1>"
      }
    )
    (Variant.default (RawServer { status: internalServerError500, headers: mempty, body: HtmlString $ "<h1> I wasn't able to handle the resposnse.. </h1>"}))
  )
  <<< Okayish.toVariant
  $ res
htmlResponse _ = RawServer
  { status: ok200
  , headers: mempty
  , body: HtmlString $ "<h1>" <> "Ongoing request" <> "</h1>"
  }

web =
  root
    { shop: requestDuplex /\ (responseDuplex : HNil)
    , admin: requestDuplex /\ (Rendered responseDuplex htmlResponse : HNil)
    , sub:
        { shop: Method
          { "GET": requestDuplex /\ (responseDuplex : HNil)
          , "POST": requestDuplex /\ (responseDuplex : HNil)
          }
        }
    }

handlers =
  { shop: { "application/json":
      (const $ pure $ Okayish.Type.fromEither $ Right { a: 8, b: "test", method: "ANY" })}
  , admin: { "application/json": (\r → do
      i ← liftEffect random
      -- | TODO: Why this doesn't work??
      -- pure $ (Okayish.Type.fromEither $ Right { a: r.productId, b: show i, method: "LJL" }) ∷ Aff (Okayish (fail ∷ Number) _))}
      pure $ (Okayish.Type.fromEither $ Right { a: r.productId, b: show i, method: "LJL" }) ∷ Aff (Okayish () _))}
  , sub:
    { shop:
      { "GET": { "application/json": \r → pure $ Okayish.fromEither $ Right { a: r.productId, b: "sub-test", method: "received GET" } }
      , "POST": { "application/json": \r → pure $ Okayish.fromEither $ Right { a: r.productId, b: "sub-test", method: "received POST" }}
      }
    }
  }

print (Spec.Spec { request: reqDpl }) = Request.print reqDpl

req = do
  let
    Spec { api } = web
  specRequestBuilder api

main :: Effect Unit
main = do
  let
    handlers' = renderToApi web handlers
    Spec { api } = web
  onClose ← Node.Server.serve api handlers' { hostname: "127.0.0.1", port: 9000, backlog: Nothing } (log "127.0.0.1:9000")
  onClose (log "Closed")

  log $ _.path <<< print api $ req.admin."application/json" { productId: 130 }
  -- log $ _.path <<< print api $ req.sub.shop."GET"."application/json" {productId: 8}
  log $ unsafeStringify <<< print api $ req.shop."application/json" { productId: 8 }

