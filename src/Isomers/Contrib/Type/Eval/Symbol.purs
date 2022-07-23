module Isomers.Contrib.Type.Eval.Symbol where

-- import Prim.Boolean (False, True, kind Boolean)

import Prim.Boolean (False, True)
import Prim.Symbol (class Append, class Cons) as Symbol
import Type.Eval (class Eval, TypeExpr)

-- | Strips existing prefix. Failes if a value is not a prefix.
foreign import data StripPrefix :: Symbol -> Symbol -> TypeExpr Symbol

instance evalStripPrefix ::
  ( Symbol.Append prefix suffix symbol
  ) =>
  Eval (StripPrefix prefix symbol) suffix

class IsPrefixStep :: Symbol -> Symbol -> Boolean -> Constraint
class IsPrefixStep p s r | p s -> r

instance isPrefixStep :: (Symbol.Cons ph pt p, Symbol.Cons sh st s, IsPrefixStep' ph pt sh st r) => IsPrefixStep p s r

class IsPrefixStep' :: Symbol -> Symbol -> Symbol -> Symbol -> Boolean -> Constraint
class IsPrefixStep' ph pt sh st r | ph pt sh st -> r

instance isPrefixStepEnd' :: IsPrefixStep' h "" h st True
else instance isPrefixStepEq' ::
  ( Symbol.Cons ph pt' pt
  , Symbol.Cons sh st' st
  , IsPrefixStep' ph pt' sh st' r
  ) =>
  IsPrefixStep' h pt h st r
else instance isPrefixStepNonEq' :: IsPrefixStep' h pt h' st False

foreign import data IsPrefixOf :: Symbol -> Symbol -> TypeExpr Boolean

instance evalIsPrefixOf ::
  ( IsPrefixStep prefix symbol b
  ) =>
  Eval (IsPrefixOf prefix symbol) b

