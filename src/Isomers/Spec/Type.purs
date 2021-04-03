module Isomers.Spec.Type where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, hmap)
import Isomers.Contrib.Heterogeneous.HEval ((<<<), type (<<<)) as H
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mapping.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mapping
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mapping.Record
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomers.Contrib.Type.Eval.Foldable (Foldl')
import Isomers.Request (Duplex(..)) as Request
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
  = Mappings.Record.Get "request" H.<<< Mappings.Newtype.Unwrap

_RequestMapping ∷ RequestMapping
_RequestMapping = Mappings.Record.Get _request H.<<< Mappings.Newtype.Unwrap

type ResponseMapping = Mappings.Record.Get "response" H.<<< Mappings.Newtype.Unwrap

_ResponseMapping ∷ ResponseMapping
_ResponseMapping = Mappings.Record.Get _response H.<<< Mappings.Newtype.Unwrap

-- duplex l dpl (Spec { request, response }) = Spec
--   { 

-- -- --   HMap RequestMapping t227 { | t239 } ⇒

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
