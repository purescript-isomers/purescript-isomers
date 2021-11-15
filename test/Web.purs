module Test.Web where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V(..))
import Data.Variant (Variant)
import Data.Variant (case_, default, inj, on) as Variant
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (random)
import Heterogeneous.Folding (class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (hmap, hmapWithIndex)
import Isomers.Client (RequestBuildersStep(..), requestBuilders)
import Isomers.Client (client) as Client
import Isomers.Client.Fetch (Scheme(..))
import Isomers.Client.Fetch (fetch) as Fetch
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.Contrib.Heterogeneous.List (HNil(..), (:))
import Isomers.Contrib.Type.Eval.Foldings (HCons', HList, HListProxy(..), HNil')
import Isomers.HTTP (Exchange(..))
import Isomers.HTTP (Exchange(..)) as HTTP
import Isomers.HTTP.ContentTypes (HtmlMime, _html, _json)
import Isomers.HTTP.Request (Method(..))
import Isomers.Node.Server (serve) as Node.Server
import Isomers.Request (Accum(..))
import Isomers.Request (Duplex(..)) as Request
import Isomers.Request.Accum (insert, print) as Request.Accum
import Isomers.Request.Duplex (body, int, print, segment, string) as Request.Duplex
import Isomers.Request.Duplex (int, segment) as Request.Duplex.Record
import Isomers.Request.Duplex.Parser (int) as Parser
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
import Isomers.Spec (BuilderStep(..), accumSpec, client, requestBuilders, rootAccumSpec) as Spec
import Isomers.Spec (Spec(..))
import Isomers.Spec.Accept (accumSpec, requestAccum) as Accept
import Isomers.Spec.Builder (WithBody(..), withBody)
import Isomers.Spec.Builder (insert) as Web.Builder
import Isomers.Web (requestBuilders) as Web
import Isomers.Web (toSpec)
import Isomers.Web.Builder (Rendered(..))
import Isomers.Web.Builder (webSpec)
import Isomers.Web.Client.Render (ContractRequest(..), RenderStep(..))
import Isomers.Web.Client.Router (webRouter, webRequest) as Web.Client.Router
import Isomers.Web.Server (renderToApi)
import Isomers.Web.Types (WebSpec(..))
import JS.Unsafe.Stringify (unsafeStringify)
import Network.HTTP.Types (internalServerError500, ok200)
import Node.Stream (onClose)
import Polyform.Batteries.Json.Duals ((:=))
import Polyform.Batteries.Json.Duals (Pure, arrayOf, int, number, object, string) as Json.Duals
import Polyform.Dual (Dual(..))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Reporter (R)
import Polyform.Validator.Dual (runSerializer, runValidator)
import Polyform.Validator.Dual.Pure (runSerializer, runValidator) as Dual.Pure
import React.Basic.Hooks (Component)
import Type.Eval.Dispatch (KindOf)
import Type.Prelude (Proxy(..), Proxy(..))

responseDuplex = responseDual d
  where
  d ∷ Json.Duals.Pure _ { a ∷ Int, b ∷ String, method ∷ String }
  d = Json.Duals.object >>> rec
    where
    rec =
      Dual.Record.build
        $ (Proxy ∷ Proxy "a")
        := Json.Duals.int
        <<< (Proxy ∷ Proxy "b")
        := Json.Duals.string
        <<< (Proxy ∷ Proxy "method")
        := Json.Duals.string

responseDual d = Response.Okayish.Duplexes.asJson ser prs
  where
  ser = Dual.Pure.runSerializer d

  prs = lmap unsafeStringify <<< un V <<< Dual.Pure.runValidator d

newtype HTML
  = HTML String

get view = Method { "GET": view }

htmlResponse (_ /\ (Exchange _ (Just (Right res)))) =
  ( Variant.on _ok
      ( \r →
          RawServer
            { status: ok200
            , headers: mempty
            , body: HtmlString $ "<h1>" <> unsafeStringify r <> "</h1>"
            }
      )
      (Variant.default (RawServer { status: internalServerError500, headers: mempty, body: HtmlString $ "<h1> I wasn't able to handle the resposnse.. </h1>" }))
  )
    <<< Okayish.toVariant
    $ res

htmlResponse _ =
  RawServer
    { status: ok200
    , headers: mempty
    , body: HtmlString $ "<h1>" <> "Ongoing request" <> "</h1>"
    }

render = htmlResponse

testString = Web.Builder.insert (Proxy ∷ Proxy "test") (Request.Duplex.string Request.Duplex.segment)

---------------------------------------------
z :: forall t1 t4. Accum t4 t1 t1 t1
z = Accum (pure identity) identity

bodyString = (Request.Duplex.body (Proxy ∷ Proxy "str") (const mempty)) ∷ Request.Duplex (str ∷ Fiber String) Int String

-- type X ∷ HList Symbol
-- type X = HNil'
-- 
-- type X' ∷ Type
-- type X' = KindOf "test"
-- 
-- type X'' ∷ HList Symbol
-- type X'' = HCons' "test" HNil'


shop =
  Spec.rootAccumSpec
    $ Spec.accumSpec Spec.BuilderStep
        { x: withBody (Proxy ∷ Proxy "payload") bodyString /\ responseDuplex
        , y: responseDuplex : HNil
        }

x =
  Spec.accumSpec Spec.BuilderStep
    { shop: z /\ responseDuplex
    , admin: z /\ (responseDuplex : HNil)
    , sub:
        { shop:
            Method
              { "GET": responseDuplex
              , "POST": responseDuplex : HNil
              }
        }
    }

-- client :: { "" :: { "application/json" :: { productId :: Int
--                                  }
--                                  -> Aff
--                                       (Either Error
--                                          (Okayish ()
--                                             { a :: Int
--                                             , b :: String
--                                             , method :: String
--                                             }
--                                          )
--                                       )
--          , "text/html" :: { productId :: Int
--                           }
--                           -> Aff (Either Error (RawClient HtmlString))
--          }
--  }
-- client = Spec.client (Fetch.fetch hostInfo) $ toSpec web

-- -- web :: forall t738 t898.
-- --    WebSpec t738
-- --      (HJust
-- --         { "" :: Tagged "application/json"
-- --                   (Renderer t898
-- --                      { productId :: Int
-- --                      }
-- --                      (Okayish ()
-- --                         { a :: Int
-- --                         , b :: String
-- --                         , method :: String
-- --                         }
-- --                      )
-- --                      (RawServer HtmlString)
-- --                   )
-- --         }
-- --      )
-- --      (Variant
-- --         ( "" :: Variant
-- --                   ( "application/json" :: { productId :: Int
-- --                                           }
-- --                   , "text/html" :: { productId :: Int
-- --                                    }
-- --                   )
-- --         )
-- --      )
-- --      (Variant
-- --         ( "" :: Variant
-- --                   ( "application/json" :: { productId :: Int
-- --                                           }
-- --                   , "text/html" :: { productId :: Int
-- --                                    }
-- --                   )
-- --         )
-- --      )
-- --      { "" :: { "application/json" :: Duplex "application/json"
-- --                                        (Okayish ()
-- --                                           { a :: Int
-- --                                           , b :: String
-- --                                           , method :: String
-- --                                           }
-- --                                        )
-- --                                        (Okayish ()
-- --                                           { a :: Int
-- --                                           , b :: String
-- --                                           , method :: String
-- --                                           }
-- --                                        )
-- --              , "text/html" :: Duplex "text/html" (RawServer HtmlString) (RawClient HtmlString)
-- --              }
-- --      }
web = do
  webSpec
    $ Web.Builder.insert (Proxy ∷ Proxy "productId") (Request.Duplex.int Request.Duplex.segment)
        { "": z /\ (Rendered responseDuplex htmlResponse : HNil) --responseDuplex
        , shop: z /\ (responseDuplex : HNil)
        , admin: z /\ (responseDuplex : HNil)
        , sub:
            testString
              { shop:
                  Method
                    { "GET": z /\ (Rendered responseDuplex htmlResponse : HNil)
                    , "POST": z /\ (Rendered (responseDual Json.Duals.number) htmlResponse : HNil)
                    }
              }
        }

handlers =
  { "": { "application/json": (const $ pure $ Okayish.Type.fromEither $ Right { a: 8, b: "test", method: "ANY" }) }
  , shop:
      { "application/json":
          (const $ pure $ Okayish.Type.fromEither $ Right { a: 8, b: "test", method: "ANY" })
      }
  , admin:
      { "application/json":
          ( \r → do
              i ← liftEffect random
              -- | TODO: Why this doesn't work??
              -- pure $ (Okayish.Type.fromEither $ Right { a: r.productId, b: show i, method: "LJL" }) ∷ Aff (Okayish (fail ∷ Number) _))}
              pure $ (Okayish.Type.fromEither $ Right { a: r.productId, b: show i, method: "LJL" }) ∷ Aff (Okayish () _)
          )
      }
  , sub:
      { shop:
          { "GET":
              { "application/json":
                  \r → do
                    liftEffect $ log "REQUEST"
                    liftEffect $ log $ unsafeStringify r
                    pure $ Okayish.fromEither $ Right { a: r.productId, b: "sub-test:" <> r.test, method: "received GET" }
              }
          , "POST": { "application/json": \r → pure $ Okayish.fromEither $ Right 8.0 }
          }
      }
  }

req = Web.requestBuilders web

webReq = Web.Client.Router.webRequest web

-- webRouter = req.""."application/json" { productId: 8 }
doc = htmlResponse (unit /\ (HTTP.Exchange (req.""."application/json" { productId: 8 }) Nothing))

hostInfo = { hostName: "127.0.0.1", port: 9000, scheme: HTTP }

webRouter = Web.Client.Router.webRouter { doc } web hostInfo

printRoute (WebSpec { spec: Spec { request: reqDpl } }) = Request.Duplex.print reqDpl

main :: Effect Unit
main = do
  let
    handlers' = renderToApi web handlers pure unit

    WebSpec { spec } = web
  onClose ← Node.Server.serve spec handlers' identity { hostname: "127.0.0.1", port: 9000, backlog: Nothing } (log "127.0.0.1:9000")
  onClose (log "Closed")
  log $ _.path <<< printRoute web $ req.""."application/json" { productId: 130 }
  launchAff_
    $ do
        delay $ Milliseconds 2000.0
        liftEffect $ log $ "RESULT:"

-- result ← client.sub.shop."GET"."application/json" { productId: 120, test: "request-from-client" }
-- liftEffect $ log $ unsafeStringify $ result
-- log $ _.path <<< print web $ req.sub.shop."GET"."application/json" {productId: 8}
-- log $ unsafeStringify <<< printRoute web $ req.shop."application/json" { productId: 8 }
