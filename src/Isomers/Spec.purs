module Isomers.Spec
  ( module Builder
  )
  where

import Prelude

import Isomers.Spec.Builder (spec, class Builder) as Builder

import Control.Alt ((<|>))
import Data.Lens (Iso, Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, on)
import Data.Variant (class Contractable, contract, expand, inj) as Variant
import Data.Variant.Prefix (NilExpr, PrefixStep, UnprefixStep)
import Data.Variant.Prefix (PrefixCases, UnprefixCases) as Data.Variant.Prefix
import Data.Variant.Prefix (add, remove) as Variant.Prefix
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap)
import Isomers.Contrib.Heterogeneous (hmap')
import Isomers.Contrib.Heterogeneous.Foldings (Flatten(..)) as Heterogeneous.Foldings
import Isomers.Contrib.Heterogeneous.Mappings (Compose(..)) as Mappings
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomers.HTTP (Method(..)) as Isomers.HTTP
import Isomers.HTTP.Method (Method) as HTTP
import Isomers.Spec.Builder (spec) as Spec.Builder
import Isomers.Spec.Request.Data (_Data)
import Prim.Row (class Cons, class Union) as Row
import Prim.RowList (class RowToList)
import Prim.Symbol (class Append) as Symbol
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex (prefix) as Request.Duplex
import Request.Duplex.Generic.Variant (class MethodPrefixRoutes, class VariantParser, class VariantPrinter, methodVariant) as Request.Duplex.Generic.Variant
import Request.Duplex.Parser (RequestParser(..), RouteError(..), RouteResult(..)) as Request.Duplex.Parser
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter(..))
import Type.Eval (class Eval)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Type.Row (RProxy)

-- | XXX: New Spec migration
-- newtype Spec request response
--   = Spec
--   { request ∷ RequestDuplex' request
--   , response ∷ response
--   }
-- 
-- derive instance newtypeSpec ∷ Newtype (Spec req res) _
-- 
-- _request = SProxy ∷ SProxy "request"
-- 
-- _response = SProxy ∷ SProxy "response"
-- 
-- type RequestMapping
--   = Mappings.Compose (Mappings.Record.Get "request") Mappings.Newtype.Unwrap
-- 
-- _RequestMapping ∷ RequestMapping
-- _RequestMapping = Mappings.Record.Get _request `Mappings.Compose` Mappings.Newtype.Unwrap
-- 
-- type ResponseMapping
--   = Mappings.Compose (Mappings.Record.Get "response") Mappings.Newtype.Unwrap
-- 
-- _ResponseMapping ∷ ResponseMapping
-- _ResponseMapping = Mappings.Record.Get _response `Mappings.Compose` Mappings.Newtype.Unwrap
-- 
-- method ∷
--   ∀ t221 t227 t235 t238 t239.
--   HMap ResponseMapping t227 { | t221 } ⇒
--   HMap RequestMapping t227 { | t239 } ⇒
--   RowToList t239 t238 ⇒
--   Request.Duplex.Generic.Variant.VariantParser t238 t239 t235 ⇒
--   Request.Duplex.Generic.Variant.VariantPrinter t238 t239 t235 ⇒
--   Request.Duplex.Generic.Variant.MethodPrefixRoutes t238 t239 ⇒
--   t227 →
--   Spec (Isomers.HTTP.Method (Variant t235)) (Isomers.HTTP.Method { | t221 })
-- method r = Spec { request, response }
--   where
--   -- | Drop `Spec` from the values
--   requests = hmap' _RequestMapping r
-- 
--   -- | TODO: Move to HTTP.Method
--   _Method ∷ ∀ a. Iso' (HTTP.Method a) a
--   _Method = _Newtype
-- 
--   request = _Method (Request.Duplex.Generic.Variant.methodVariant requests)
-- 
--   response = Isomers.HTTP.Method (hmap' _ResponseMapping r)
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
-- emptyVariantSpec ∷ Spec (Variant ()) {}
-- emptyVariantSpec = Spec { request: emptyVariantDuplex, response: {} }
--   where
--   emptyVariantDuplex ∷ RequestDuplex' (Variant ())
--   emptyVariantDuplex = RequestDuplex mempty fail
--     where
--     fail ∷ RequestParser (Variant ())
--     fail = Request.Duplex.Parser.Chomp $ const $ Request.Duplex.Parser.Fail Request.Duplex.Parser.EndOfPath
-- 
-- -- | We recurse into the records when we encounter them as field value.
-- -- | Finally this result should be wrapped into the `RecordCodecs` constructor.
-- -- | This case is buggy
-- instance specFoldingRec ∷
--   ( HFoldlWithIndex (SpecFolding sep) (Spec (Variant ()) (Record ())) { | r } r'
--   , FoldingWithIndex (SpecFolding sep) l acc r' (Spec req res)
--   ) ⇒
--   FoldingWithIndex (SpecFolding sep) l acc { | r } (Spec req res) where
--   foldingWithIndex pref l acc r = do
--     let
--       r' = hfoldlWithIndex pref emptyVariantSpec r
--     foldingWithIndex pref l acc r'
-- 
-- -- | We split this folding into separate foldings over request and response codecs rows.
-- instance specFoldingSpec ∷
--   ( FoldingWithIndex (Heterogeneous.Foldings.Flatten sep) (SProxy l) resAcc res res'
--   , FoldingWithIndex (SpecFolding sep) (SProxy l) (RequestDuplex' reqAcc) (RequestDuplex' req) (RequestDuplex' req')
--   ) ⇒
--   FoldingWithIndex
--     (SpecFolding sep)
--     (SProxy l)
--     (Spec reqAcc resAcc)
--     (Spec req res)
--     (Spec req' res') where
--   foldingWithIndex sf@(SpecFolding sep _) l (Spec acc) (Spec { request, response }) = do
--     let
--       request' = foldingWithIndex sf l acc.request request
-- 
--       response' = foldingWithIndex (Heterogeneous.Foldings.Flatten sep) l acc.response response
--     Spec { request: request', response: response' }
-- 
-- 
-- instance hfoldlWithIndexSpec ∷
--   HFoldlWithIndex (SpecFolding sep) acc (Spec request response) (Spec request response) where
--   hfoldlWithIndex _ _ r = r
-- 
-- 
-- endpoint ∷ ∀ t38 t40. RequestDuplex' t38 → t40 → Spec (Spec.Request.Data t38) t40
-- endpoint request response = Spec { request: request', response }
--   where
--   -- _Newtype ∷ ∀ t a s b. Newtype t a ⇒ Newtype s b ⇒ Iso t s a b
--   request' = _Data request
-- 
-- 
