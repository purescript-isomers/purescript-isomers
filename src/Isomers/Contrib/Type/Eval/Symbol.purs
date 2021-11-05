module Isomers.Contrib.Type.Eval.Symbol where

-- import Prim.Boolean (False, True, kind Boolean)

import Prim.Boolean (False, True)
import Prim.Symbol (class Append, class Cons) as Symbol
import Type.Eval (class Eval, TypeExpr)
import Type.Prelude (Proxy)

-- | Strips existing prefix. Failes if a value is not a prefix.
foreign import data StripPrefix ∷ Type → Type → TypeExpr

instance evalStripPrefix ∷
  (Symbol.Append prefix suffix symbol) ⇒
  Eval (StripPrefix (Proxy prefix) (Proxy symbol)) (Proxy suffix)

class IsPrefixStep (p ∷ Symbol) (s ∷ Symbol) (r ∷ Boolean) | p s → r
instance isPrefixStep ∷ (Symbol.Cons ph pt p, Symbol.Cons sh st s, IsPrefixStep' ph pt sh st r) ⇒ IsPrefixStep p s r

class IsPrefixStep' (ph ∷ Symbol) (pt ∷ Symbol) (sh ∷ Symbol) (st ∷ Symbol) (r ∷ Boolean)| ph pt sh st → r
instance isPrefixStepEnd' :: IsPrefixStep' h "" h st True
else instance isPrefixStepEq' :: (Symbol.Cons ph pt' pt, Symbol.Cons sh st' st, IsPrefixStep' ph pt' sh st' r) => IsPrefixStep' h pt h st r
else instance isPrefixStepNonEq' :: IsPrefixStep' h pt h' st False

foreign import data IsPrefixOf ∷ Type → Type → TypeExpr

instance evalIsPrefixOf ∷
  ( IsPrefixStep prefix symbol b) ⇒ Eval (IsPrefixOf (Proxy prefix) (Proxy symbol)) (Proxy b)

