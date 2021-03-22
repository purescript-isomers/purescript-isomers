module Isomers.Contrib.Data.Variant where

import Prelude

import Data.Maybe (isJust)
import Data.String (Pattern(..), stripPrefix) as String
import Data.Variant (Unvariant(..), Variant, unvariant)
import Isomers.Contrib.Type.Eval.Foldable (SomeWithIndex)
import Isomers.Contrib.Type.Eval.Symbol (IsPrefixOf)
import Isomers.Contrib.Type.Eval.Tuple (Curry')
import Prim.Boolean (True)
import Type.Eval (class Eval)
import Type.Eval.Boolean (BProxy)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow)
import Type.Eval.Tuple (Fst)
import Type.Prelude (class IsSymbol, RProxy, SProxy, reflectSymbol)

type IsPrefixOfSome (prefix ∷ Symbol) (row ∷ # Type) = (SomeWithIndex (Curry' (IsPrefixOf (SProxy prefix) <<< Fst)) <<< FromRow) (RProxy row)

matchesTagPrefix ∷
  ∀ prefix req.
  Eval (IsPrefixOfSome prefix req) (BProxy True) ⇒
  IsSymbol prefix ⇒
  SProxy prefix →
  Variant req →
  Boolean
matchesTagPrefix p v =
  let
    Unvariant c = unvariant v
  in
    c \s _ → isJust (String.stripPrefix (String.Pattern $ reflectSymbol p) (reflectSymbol s))

tag ∷ ∀ v. Variant v → String
tag v = do
  let
    Unvariant c = unvariant v
  c \s _ → reflectSymbol s

