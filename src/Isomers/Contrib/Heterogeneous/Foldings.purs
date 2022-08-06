module Isomers.Contrib.Heterogeneous.Foldings where

import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Prim.Symbol (class Append) as Symbol
import Record (get, insert, union) as Record
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow)
import Type.Prelude (class IsSymbol, Proxy(Proxy))

newtype Flatten (sep :: Symbol) = Flatten (Proxy sep)

-- | TODO: Cover `Variant` too.
instance
  ( HFoldlWithIndex (Record.Prefix.PrefixProps sym) (Record.Builder.Builder {} {}) { | res }
      (Record.Builder.Builder {} { | res' })
  , Symbol.Append l sep sym
  , Row.Union res' acc acc'
  ) =>
  FoldingWithIndex
    (Flatten sep)
    (Proxy l)
    ({ | acc })
    ({ | res })
    ({ | acc' }) where
  foldingWithIndex _ _ acc v = do
    let
      sym = Proxy :: Proxy sym
    Record.union (Record.Prefix.add sym v) acc
else instance
  ( IsSymbol l
  , Row.Lacks l acc
  , Row.Cons l a acc acc'
  ) =>
  FoldingWithIndex
    (Flatten sep)
    (Proxy l)
    { | acc }
    a
    { | acc' } where
  foldingWithIndex _ l acc v = Record.insert l v acc

foreign import data ToRowListStep :: Type -> TypeExpr Type

instance (RowToList rec rl) => Eval (ToRowListStep { | rec }) (Proxy rl)
else instance Eval (ToRowListStep a) (Proxy a)


