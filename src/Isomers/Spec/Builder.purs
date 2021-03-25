module Isomers.Spec.Builder where

import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class HFoldl, class HFoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Isomers.Contrib.Heterogeneous.List (type (:))
import Isomers.HTTP.Request.Headers.Accept (MediaPattern)
import Isomers.Response (Duplex) as Response
import Isomers.Request (Duplex) as Request
import Isomers.Spec.Accept (Accept, RequestParserFolding, RequestPrinterFolding, ResponseContentTypesMapping, ResponseRecFolding)
import Isomers.Spec.Accept (spec) as Spec.Accept
import Isomers.Spec.Record (SpecFolding, spec) as Spec.Record
import Isomers.Spec.Record (SubspecInput)
import Isomers.Spec.Spec (Spec(..))
import Prim.RowList (class RowToList)
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter)
import Type.Eval (class Eval)

-- | Used for recursive mapping over a record fields
data SpecStep = SpecStep

instance mappingSpecMapping ∷ Builder a i req res ⇒ Mapping SpecStep a (Spec i req res) where
  mapping _ a = spec a

class Builder a i req res | a → i req res where
  spec ∷ a → Spec i req res

-- | For more info regarding this processing please check
-- | docs for `Spec.Accept.response` and `Spec.Accept.request`.
instance builderAcceptSpec ∷
  ( HMap ResponseContentTypesMapping (h : t) cts
  , HFoldl (RequestParserFolding r req)
      (MediaPattern -> RequestParser (r -> Variant ()))
      cts
      (MediaPattern -> RequestParser (r -> Variant vReq))
  , HFoldl ResponseRecFolding {} (h : t) res
  , HFoldl
    (RequestPrinterFolding req)
    (Variant () → RequestPrinter)
    cts
    (Variant vReq → RequestPrinter)
  ) ⇒ Builder (Request.Duplex r req req /\ (h : t)) r (Accept (Variant vReq)) (Accept res) where
  spec = Spec.Accept.spec
-- | For a tuple spec which doesn't care about accept header.
else instance builderPlainEndpoint ∷
  Builder (Request.Duplex r req req /\ Response.Duplex res res) r req (Response.Duplex res res) where
  spec (request /\ response) = Spec { request, response }

instance builderPlainSpec ∷
  Builder (Spec i req res) i req res where
  spec s = s

-- | We map over a record so we have record of Specs.
-- | Next we fold the record to get the final Spec.
instance recBuilderRecord ∷
  ( HMap SpecStep { | r } { | r' }
  , RowToList r' rl'
  , Eval (SubspecInput r') i
  , HFoldlWithIndex Spec.Record.SpecFolding (Spec i (Variant ()) {}) { | r' } (Spec i req res)
  ) ⇒
  Builder { | r } i req res where
  spec r = Spec.Record.spec true r'
    where
      r' = hmap SpecStep r

