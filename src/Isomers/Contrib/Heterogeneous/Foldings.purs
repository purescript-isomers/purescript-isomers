module Isomers.Contrib.Heterogeneous.Foldings where

import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.Symbol (class Append) as Symbol
import Record (insert, union) as Record
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Type.Prelude (class IsSymbol, Proxy(Proxy))

newtype Flatten (sep :: Symbol) = Flatten (Proxy sep)

-- | TODO: Cover `Variant` too.
instance flattenRecordRec ::
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
  foldingWithIndex (Flatten sep) l acc v = do
    let
      sym = Proxy :: Proxy sym
    Record.union (Record.Prefix.add sym v) acc
else instance flattenRecord ::
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
  foldingWithIndex pref l acc v = Record.insert l v acc
