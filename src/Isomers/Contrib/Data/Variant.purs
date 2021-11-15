module Isomers.Contrib.Data.Variant where

import Prelude

import Data.Maybe (isJust)
import Data.String (Pattern(..), stripPrefix) as String
import Data.Variant (Unvariant(..), Variant, unvariant)
import Isomers.Contrib.Type.Eval.Foldable (SomeWithIndex)
import Isomers.Contrib.Type.Eval.Symbol (IsPrefixOf)
-- import Isomers.Contrib.Type.Eval.Tuple (Curry')
import Prim.Boolean (True)
import Prim.Row (class Cons, class Lacks) as Row
import Type.Eval (class Eval)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow)
import Type.Eval.Tuple (Fst)
import Type.Prelude (class IsSymbol, Proxy, reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- type IsPrefixOfSome (prefix ∷ Symbol) (row ∷ Row Type) = (SomeWithIndex (Curry' (IsPrefixOf (Proxy prefix) <<< Fst)) <<< FromRow) (Proxy row)
-- 
-- matchesTagPrefix ∷
--   ∀ prefix req.
--   Eval (IsPrefixOfSome prefix req) (Proxy True) ⇒
--   IsSymbol prefix ⇒
--   Proxy prefix →
--   Variant req →
--   Boolean
-- matchesTagPrefix p v =
--   let
--     Unvariant c = unvariant v
--   in
--     c \s _ → isJust (String.stripPrefix (String.Pattern $ reflectSymbol p) (reflectSymbol s))

tag ∷ ∀ v. Variant v → String
tag v = do
  let
    Unvariant c = unvariant v
  c \s _ → reflectSymbol s

append ∷ ∀ a l v v'. Row.Lacks l v ⇒ Row.Cons l a v v' ⇒ Proxy l → Variant v → Variant v'
append _ v = unsafeCoerce v

