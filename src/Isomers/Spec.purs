module Isomers.Spec
  ( module Builder
  , module Type
  ) where

import Prelude
import Isomers.Spec.Builder (root, spec, class Builder) as Builder
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
import Isomers.HTTP (Method(..)) as HTTP
import Isomers.Spec.Builder (spec) as Spec.Builder
import Isomers.Spec.Type (Spec(..)) as Type
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

--- method ∷
---   ∀ t221 t227 t235 t238 t239.
---   HMap ResponseMapping t227 { | t221 } ⇒
---   HMap RequestMapping t227 { | t239 } ⇒
---   RowToList t239 t238 ⇒
---   Request.Duplex.Generic.Variant.VariantParser t238 t239 t235 ⇒
---   Request.Duplex.Generic.Variant.VariantPrinter t238 t239 t235 ⇒
---   Request.Duplex.Generic.Variant.MethodPrefixRoutes t238 t239 ⇒
---   t227 →
---   Spec (Isomers.HTTP.Method (Variant t235)) (Isomers.HTTP.Method { | t221 })
--- method r = Spec { request, response }
---   where
---   -- | Drop `Spec` from the values
---   requests = hmap' _RequestMapping r
--- 
---   -- | TODO: Move to HTTP.Method
---   _Method ∷ ∀ a. Iso' (HTTP.Method a) a
---   _Method = _Newtype
--- 
---   request = _Method (Request.Duplex.Generic.Variant.methodVariant requests)
--- 
---   response = Isomers.HTTP.Method (hmap' _ResponseMapping r)
