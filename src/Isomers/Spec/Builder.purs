module Isomers.Spec.Builder where

import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class HFoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Isomers.Contrib.Heterogeneous.List (type (:))
import Isomers.HTTP.Request.Headers.Accept (MediaPattern)
import Isomers.Request (Duplex, Parser, Printer) as Request
import Isomers.Request.Duplex (root) as Request.Duplex
import Isomers.Request.Duplex.Generic (class HFoldlVariantStep)
import Isomers.Response (Duplex) as Response
import Isomers.Spec.Accept (Accept, RequestParserFolding, RequestPrinterFolding, ResponseContentTypesMapping)
import Isomers.Spec.Accept (ResponseRecordStep) as Accept
import Isomers.Spec.Accept (spec) as Spec.Accept
import Isomers.Spec.Record (SubspecInput, SubspecBody)
import Isomers.Spec.Record (spec) as Spec.Record
import Isomers.Spec.Type (ResponseMapping, Spec(..), RequestMapping)
import Type.Eval (class Eval)
import Type.Prelude (RProxy)

-- | Used for recursive mapping over a record fields
data SpecStep = SpecStep

instance mappingSpecMapping ∷ Builder a body i req res ⇒ Mapping SpecStep a (Spec body i req res) where
  mapping _ a = spec a

class Builder a body i req res | a → body i req res where
  spec ∷ a → Spec body i req res

-- | For more info regarding this processing please check
-- | docs for `Spec.Accept.response` and `Spec.Accept.request`.
instance builderAcceptSpec ∷
  ( HMap ResponseContentTypesMapping (h : t) cts
  , HFoldl (RequestParserFolding body r req)
      (MediaPattern -> Request.Parser body (r -> Variant ()))
      cts
      (MediaPattern -> Request.Parser body (r -> Variant vReq))
  , HFoldl Accept.ResponseRecordStep {} (h : t) res
  , HFoldl
    (RequestPrinterFolding req)
    (Variant () → Request.Printer)
    cts
    (Variant vReq → Request.Printer)
  ) ⇒ Builder (Request.Duplex body r req req /\ (h : t)) body r (Accept (Variant vReq)) (Accept res) where
  spec = Spec.Accept.spec
-- | For a tuple spec which doesn't care about accept header.
else instance builderPlainEndpoint ∷
  Builder (Request.Duplex body r req req /\ Response.Duplex res res) body r req (Response.Duplex res res) where
  spec (request /\ response) = Spec { request, response }

instance builderPlainSpec ∷
  Builder (Spec body i req res) body i req res where
  spec s = s

-- | We map over a record so we get record of Specs.
-- | Next we fold the record to get the final Spec.
instance recBuilderRecord ∷
  ( HMap SpecStep { | rec } { | rec' }
  , Eval (SubspecInput rec') i
  , Eval (SubspecBody rec') (RProxy body)
  , HMap ResponseMapping { | rec' } { | res }
  , HMap RequestMapping { | rec' } { | reqs }
  , HFoldlVariantStep body i { | reqs } vreq vreq
  ) ⇒
  Builder { | rec } body i (Variant vreq) { | res } where
  spec r = Spec.Record.spec true r'
    where
      r' = hmap SpecStep r

root ∷ ∀ a i rb req res.  Builder a rb i req res ⇒ a → Spec rb i req res
root a = do
  let
    Spec { request, response } = spec a
    request' = Request.Duplex.root request

  Spec { request: request', response }

