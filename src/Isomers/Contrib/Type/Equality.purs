module Isomers.Contrib.Type.Equality
  ( class TypeEquals'
  , to'
  , from'
  ) where

import Record.Extra (SList)
import Type.Prelude (Proxy)

-- | This type class asserts that types `a` and `b`
-- | are equal.
-- |
-- | The functional dependencies and the single
-- | instance below will force the two type arguments
-- | to unify when either one is known.
-- |
-- | Note: any instance will necessarily overlap with
-- | `refl` below, so instances of this class should
-- | not be defined in libraries.
class TypeEquals' a b (hint :: SList) | a -> b, b -> a where
  to' :: Proxy hint -> a -> b
  from' :: Proxy hint -> b -> a

instance refl :: TypeEquals' a a hint where
  to' _ a = a
  from' _ a = a
