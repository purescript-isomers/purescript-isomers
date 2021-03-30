module Test.Request where

import Prelude
import Data.Bifunctor (lmap)
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V(..))
import Effect.Aff (Aff)
import Global.Unsafe (unsafeStringify)
import Isomers.Client (ResponseM(..), client)
import Isomers.Client (client) as Client
import Isomers.Node (ServerRequest, spec) as Node
import Isomers.Request (Duplex(..), Duplex') as Request
import Isomers.Request.Duplex.Parser (int) as Parser
import Isomers.Request.Duplex.Parser (int) as Request.Duplex
import Isomers.Request.Duplex.Record (Root, empty, intSegment, segment) as Request.Duplex.Record
import Isomers.Response (Duplex(..), Duplex') as Response
import Isomers.Response.Duplex (asJson) as Response.Duplex
import Isomers.Server (router) as Server
import Isomers.Spec (Spec(..), spec) as Spec
import Isomers.Spec (spec) as Spec.Builder
import Polyform.Batteries.Json.Duals ((:=))
import Polyform.Batteries.Json.Duals (Pure, int, object, string) as Json.Duals
import Polyform.Dual (Dual(..))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Validator.Dual (runSerializer, runValidator)
import Polyform.Validator.Dual.Pure (runSerializer, runValidator) as Dual.Pure
import Prim.Row (class Lacks) as Row
import Type.Prelude (SProxy(..))

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
  Spec.spec
    { shop: requestDuplex /\ responseDuplex
    , admin: Request.Duplex.Record.empty /\ (pure unit ∷ Response.Duplex' Unit)
    , sub:
        { shop: requestDuplex /\ responseDuplex }
    }

client = do
  let
    Spec.Spec { request: reqDpl, response: resDpls } = api
  Client.client reqDpl resDpls

router = do
  Server.router
    api
    { shop: const $ pure { a: 8, b: "test" }
    , admin: const $ pure unit
    , sub:
        { shop: const $ pure { a: 8, b: "test" } }
    }
