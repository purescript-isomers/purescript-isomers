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
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (ClientResponse')
import Isomers.Client (client) as Client
import Isomers.Client.Fetch (Scheme(..))
import Isomers.Client.Fetch as Fetch
import Isomers.Contrib.Heterogeneous.List (HNil(..), (:))
import Isomers.HTTP.ContentTypes (TextMime, _json)
import Isomers.HTTP.Exchange as HTTP.Exchange
import Isomers.HTTP.Request (Method(..))
import Isomers.Node as Node
import Isomers.Node.Server (serve) as Node.Server
import Isomers.Request.Accum as Request.Duplex.Accum
import Isomers.Request.Accum.Type (pass)
import Isomers.Request.Duplex as Request.Duplex
import Isomers.Request.Duplex.Parser (int) as Parser
import Isomers.Response (Duplex, Duplex', Okayish, OkayishDuplex') as Response
import Isomers.Response.Duplex (header, withHeaderValue) as Response.Duplex
import Isomers.Response.Okayish.Duplexes (asJson) as Response.Okayish.Duplexes
import Isomers.Response.Okayish.Type (fromEither) as Response.Okayish
import Isomers.Runtime as Runtime
import Isomers.Server (router) as Server
import Isomers.Spec (Spec)
import Isomers.Spec (foldSpec) as Spec.Builder
import Isomers.Spec as Spec
import Isomers.Spec.Flatten as Spec.Flatten
import Isomers.Spec.Types (Spec')
import JS.Unsafe.Stringify (unsafeStringify)
import Node.Stream (onClose)
import Polyform.Batteries.Json.Duals ((:=))
import Polyform.Batteries.Json.Duals (Pure, int, object, string) as Json.Duals
import Polyform.Dual (Dual(..))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Validator.Dual (runSerializer, runValidator)
import Polyform.Validator.Dual.Pure (runSerializer, runValidator) as Dual.Pure
import Type.Prelude (Proxy(..), SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Payload = { a :: Int, b :: String, method :: String }

-- x = 8
responseDuplex
  :: Response.Duplex'
       "application/json"
       ( Response.Okayish ()
           { a :: Int
           , b :: String
           , method :: String
           }
       )
responseDuplex = do

  Response.Duplex.withHeaderValue
    (CaseInsensitiveString "X-TEST")
    "TEST value"
    $ Response.Okayish.Duplexes.asJson bodySer bodyVld

bodyDual = Json.Duals.object >>> rec
  where
  rec =
    Dual.Record.build
      $
        (Proxy :: Proxy "a")
          := Json.Duals.int
          <<< (Proxy :: Proxy "b")
            := Json.Duals.string
          <<< (Proxy :: Proxy "method")
            := Json.Duals.string
bodyVld = lmap unsafeStringify <<< un V <<< Dual.Pure.runValidator bodyDual

bodySer = Dual.Pure.runSerializer bodyDual

node :: Runtime.Node
node = unsafeCoerce "node"

nodeRequest = Node.request node

requestDuplex =
  Request.Duplex.Accum.insert (Proxy :: Proxy "productId") (Request.Duplex.int Request.Duplex.segment)
    $ Request.Duplex.Accum.insert (Proxy :: Proxy "payload") (nodeRequest.jsonBody bodySer bodyVld)
    pass

--api
--  :: forall t127
--   . Spec' t127
--       ( Variant
--           ( "shop.application/json" :: { productId :: Int }
--           , "sub.shop.GET" :: { productId :: Int }
--           , "sub.shop.POST" :: { productId :: Int }
--           )
--       )
--       { "shop.application/json" ::
--           Response.Duplex' "application/json"
--             ( Response.Okayish ()
--                 { a :: Int
--                 , b :: String
--                 , method :: String
--                 }
--             )
--       , "sub.shop.GET" ::
--           Response.Duplex' "application/json"
--             ( Response.Okayish ()
--                 { a :: Int
--                 , b :: String
--                 , method :: String
--                 }
--             )
--       , "sub.shop.POST" ::
--           Response.Duplex' "application/json"
--             ( Response.Okayish ()
--                 { a :: Int
--                 , b :: String
--                 , method :: String
--                 }
--             )
--       }
api =
  Spec.Flatten.flatten $ Spec.foldSpec
    { shop: requestDuplex /\ (responseDuplex : HNil)
    -- , admin: Request.Duplex.Record.empty /\ ((pure $ pure unit) ∷ Response.OkayishDuplex' TextMime () Unit)
    , sub:
        { shop:
            Method
              { "GET": requestDuplex /\ responseDuplex
              , "POST": requestDuplex /\ responseDuplex
              }
        }
    }


type Req = { productId :: Int, payload :: Payload }
client ::
  { "shop.application/json" :: Req -> ClientResponse' () { a :: Int, b :: String, method :: String }
  , "sub.shop.GET" :: Req -> ClientResponse' () { a :: Int , b :: String , method :: String }
  , "sub.shop.POST" :: Req -> ClientResponse' () { a :: Int, b :: String, method :: String }
  }
client = do
  let
    fetch = Fetch.fetch { hostName: "127.0.0.1", port: 8000, scheme: HTTP }
    Spec.Spec { request: reqDpl, response: resDpls } = api
  Client.client fetch reqDpl resDpls

-- handlers =
--   { shop: { "application/json": const $ pure $ Response.Okayish.fromEither $ Right { a: 8, b: "test", method: "ANY" } }
--   , admin: const $ pure (pure unit)
--   , sub:
--       { shop:
--           { "GET": \r → pure $ Response.Okayish.fromEither $ Right { a: r.productId, b: "sub-test", method: "received GET" }
--           , "POST": \r → pure $ Response.Okayish.fromEither $ Right { a: r.productId, b: "sub-test", method: "received POST" }
--           }
--       }
--   }
-- 
-- router = do
--   Server.router
--     api
--     handlers
-- 
-- z = client.shop."application/json" { productId: 8 }
-- 
-- specRequestBuilder ∷
--   ∀ body input request requestBuilders response.
--   HFoldlWithIndex (RequestBuildersStep request request) {} (Proxy request) { | requestBuilders } ⇒
--   Spec.Spec body input request response → { | requestBuilders }
-- specRequestBuilder (Spec.Spec _) = requestBuilders (Proxy ∷ Proxy request)
-- 
-- print (Spec.Spec { request: reqDpl }) = Request.print reqDpl
-- 
-- req = specRequestBuilder api
-- 
-- main :: Effect Unit
-- main = do
--   onClose ← Node.Server.serve api handlers { hostname: "127.0.0.1", port: 9000, backlog: Nothing } (log "127.0.0.1:9000")
--   onClose (log "Closed")
--   log $ _.path <<< print api $ req.admin {}
--   log $ _.path <<< print api $ req.sub.shop."GET" { productId: 8 }
--   log $ unsafeStringify <<< print api $ req.shop."application/json" { productId: 8 }
-- 
-- -- launchAff_ $ runResponseM z

-- NodeJS -> Image -> URL
-- NodeJS -> Image -> Path
-- 
-- BrowserJS -> Image -> BlobURL
