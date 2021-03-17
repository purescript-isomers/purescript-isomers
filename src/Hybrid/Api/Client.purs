module Hybrid.Api.Client where

import Prelude
import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Hybrid.HTTP.Request (Data(..)) as Request
import Hybrid.HTTP.Response.Duplex (Duplex') as Response
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList)
import Record (insert) as Record
import Request.Duplex (RequestDuplex')
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), SProxy)

-- | This folding allows us to build a request builder:
-- | a record which contains functions which builds a
-- | request value (nested Variant) which has structure
-- | corresponding to the labels path.
data RequestFolding a request
  = RequestFolding (a → request)

instance requestFoldingNewtypeVariantWrapper ::
  ( IsSymbol sym
  , Newtype (f (Variant v)) (Variant v)
  , Row.Cons sym (f (Variant v)) curr_ curr
  , RowToList v vl
  , Row.Lacks sym client
  , Row.Cons sym { | subclient } client client'
  , HFoldlWithIndex (RequestFolding (Variant v) request) {} (RLProxy vl) { | subclient }
  ) =>
  FoldingWithIndex
    (RequestFolding (Variant curr) request)
    (SProxy sym)
    { | client }
    (Proxy (f (Variant v)))
    { | client' } where
  foldingWithIndex (RequestFolding inj) prop client _ = do
    let
      inj' ∷ Variant v → request
      inj' = inj <<< Variant.inj prop <<< wrap

      subclient = hfoldlWithIndex (RequestFolding inj') {} (RLProxy ∷ RLProxy vl)
    Record.insert prop subclient client
else instance requestFoldingVariant ::
  ( IsSymbol sym
  , RowToList v vl
  , Row.Cons sym (Variant v) curr_ curr
  , Row.Lacks sym client
  , Row.Cons sym { | subclient } client client'
  , HFoldlWithIndex (RequestFolding (Variant v) request) {} (RLProxy vl) { | subclient }
  ) =>
  FoldingWithIndex
    (RequestFolding (Variant curr) request)
    (SProxy sym)
    { | client }
    (Proxy (Variant v))
    { | client' } where
  foldingWithIndex (RequestFolding inj) prop client _ = do
    let
      f ∷ Variant v → Variant curr
      f = Variant.inj prop

      inj' = inj <<< f

      subclient = hfoldlWithIndex (RequestFolding inj') {} (RLProxy ∷ RLProxy vl)
    Record.insert prop subclient client
else instance requestFoldingData ::
  ( IsSymbol sym
  , Row.Lacks sym client
  , Row.Cons sym (d → request) client client'
  , Row.Cons sym (Request.Data d) r_ r
  ) =>
  FoldingWithIndex
    (RequestFolding (Variant r) request)
    (SProxy sym)
    { | client }
    (Proxy (Request.Data d))
    { | client' } where
  foldingWithIndex (RequestFolding inj) prop client d = do
    let
      inj' = inj <<< Variant.inj prop <<< Request.Data
    Record.insert prop inj' client

instance hfoldlWithIndexRequestFoldingVariantWrapper ∷
  ( HFoldlWithIndex (RequestFolding (Variant v) request) {} (RLProxy vl) { | client }
  , Newtype (f (Variant v)) (Variant v)
  , RowToList v vl
  ) ⇒
  HFoldlWithIndex (RequestFolding (f (Variant v)) request) unit (Proxy (f (Variant v))) { | client } where
  hfoldlWithIndex (RequestFolding f) init _ = hfoldlWithIndex (RequestFolding (f <<< wrap)) {} (RLProxy ∷ RLProxy vl)
else instance hfoldlWithIndexRequestFoldingVariant ∷
  ( HFoldlWithIndex (RequestFolding (Variant v) request) {} (RLProxy vl) { | client }
  , RowToList v vl
  ) ⇒
  HFoldlWithIndex (RequestFolding (Variant v) request) unit (Proxy (Variant v)) { | client } where
  hfoldlWithIndex cf init _ = hfoldlWithIndex cf {} (RLProxy ∷ RLProxy vl)

request ∷ ∀ client t. HFoldlWithIndex (RequestFolding t t) {} (Proxy t) { | client } ⇒ Proxy t → { | client }
request = hfoldlWithIndex (RequestFolding (identity ∷ t → t)) {}

newtype ClientFolding aff a
  = ClientFolding (Response.Duplex' aff a)

derive instance newtypeClientFolding ∷ Newtype (ClientFolding aff a) _

-- instance clientFoldingNewtypeWrapper ∷ 
-- Spec (Variant request) { | resCodecs } → Exchange (Request "GET" request) res
-- -- exchange ∷ ∀ req. RequestDuplex' req → req → Aff (Hybrid.HTTP.Exchange req String)
-- 
-- post ∷ Spec (Variant request) { | resCodecs } →
