module Isomers.Spec.Type where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, hmap)
import Isomers.Contrib.Heterogeneous.Mappings (Compose(..)) as Mappings
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mapping.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mapping
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mapping.Record
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomers.Contrib.Type.Eval.Foldable (Foldl')
import Isomers.Request.Duplex (Duplex(..)) as Request
import Isomers.Request.Duplex.Generic (PrefixRoutes, variant) as Request.Duplex.Generic
import Isomers.Request.Duplex.Generic (class HFoldlVariantStep, VariantStep(..))
import Isomers.Request.Duplex.Variant (empty, injInto) as Request.Duplex.Variant
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.RowList (class RowToList)
import Record (insert) as Record
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow)
import Type.Prelude (class IsSymbol, class TypeEquals, SProxy(..))
import Type.Row (RProxy)

newtype Spec reqBody input request response
  = Spec
  { request ∷ Request.Duplex reqBody input request request
  , response ∷ response
  }

type Root reqBody = Spec reqBody {}

derive instance newtypeSpec ∷ Newtype (Spec reqBody i req res) _

_request = SProxy ∷ SProxy "request"

_response = SProxy ∷ SProxy "response"

type RequestMapping
  = Mappings.Compose (Mappings.Record.Get "request") Mappings.Newtype.Unwrap

_RequestMapping ∷ RequestMapping
_RequestMapping = Mappings.Record.Get _request `Mappings.Compose` Mappings.Newtype.Unwrap

type ResponseMapping
  = Mappings.Compose (Mappings.Record.Get "response") Mappings.Newtype.Unwrap

_ResponseMapping ∷ ResponseMapping
_ResponseMapping = Mappings.Record.Get _response `Mappings.Compose` Mappings.Newtype.Unwrap

-- -- --   HMap RequestMapping t227 { | t239 } ⇒

-- -- -- method ∷
-- -- --   ∀ t221 t227 t235 t238 t239.
-- -- --   HMap ResponseMapping t227 { | t221 } ⇒
-- -- --   HMap RequestMapping t227 { | t239 } ⇒
-- -- --   RowToList t239 t238 ⇒
-- -- --   Request.Duplex.Generic.Variant.VariantParser t238 t239 t235 ⇒
-- -- --   Request.Duplex.Generic.Variant.VariantPrinter t238 t239 t235 ⇒
-- -- --   Request.Duplex.Generic.Variant.MethodPrefixRoutes t238 t239 ⇒
-- -- --   t227 →
-- -- --   Spec (Isomers.HTTP.Method (Variant t235)) (Isomers.HTTP.Method { | t221 })
-- -- -- method r = Spec { request, response }
-- -- --   where
-- -- --   -- | Drop `Spec` from the values
-- -- --   requests = hmap' _RequestMapping r
-- -- -- 
-- -- --   -- | TODO: Move to HTTP.Method
-- -- --   _Method ∷ ∀ a. Iso' (HTTP.Method a) a
-- -- --   _Method = _Newtype
-- -- -- 
-- -- --   request = _Method (Request.Duplex.Generic.Variant.methodVariant requests)
-- -- -- 
-- -- --   response = Isomers.HTTP.Method (hmap' _ResponseMapping r)
-- 
-- type PrefixRoutes
--   = Boolean
-- 
-- -- | We reuse this folding in the context of `Web.Spec` by
-- -- | separating specific cases with namespace.
-- data SpecFolding (sep ∷ Symbol)
--   = SpecFolding (SProxy sep) PrefixRoutes
-- 
-- prefix ∷ ∀ t173 t174. HFoldlWithIndex (SpecFolding ".") (Spec (Variant ()) {}) t173 t174 ⇒ t173 → t174
-- prefix raw = hfoldlWithIndex (SpecFolding (SProxy ∷ SProxy ".") true) emptyVariantSpec raw
-- 
-- emptyVariantSpec ∷ ∀ reqBody. Spec reqBody (Variant ()) {}
-- emptyVariantSpec = Spec { request: Request.Duplex.Variant.empty, response: {} }

-- -- endpoint ∷ ∀ t38 t40. RequestDuplex' t38 → t40 → Spec (Request.Data t38) t40
-- -- endpoint request response = Spec { request: request', response }
-- --   where
-- --   -- _Newtype ∷ ∀ t a s b. Newtype t a ⇒ Newtype s b ⇒ Iso t s a b
-- --   request' = _Data request
-- 
-- 
-- 
-- -- class Build a spec | a → spec where
-- --   build ∷ a → spec
-- -- 
-- -- instance buildSpec ∷ Build (Spec req res) (Spec req res) where
-- --   build s = s
-- -- 
-- -- instance buildSpecRecord ∷
-- -- 
-- --   Build { | rec } (Spec req res)
