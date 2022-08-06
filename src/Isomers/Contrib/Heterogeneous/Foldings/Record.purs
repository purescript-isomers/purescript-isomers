module Isomers.Contrib.Heterogeneous.Foldings.Record where

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


-- | It seems overcomplicated but I wasn't able to handle empty nested records
-- | in a better way than marking values first (with RowList) and later on
-- | fold them.
-- | That is why I fold over `RowList` with this marking but acutally carry over
-- | the record in the step carrier.
data FlattenRecStep (sep :: Symbol) record = FlattenRecStep (Proxy sep) { | record }

instance
  ( IsSymbol l
  , Row.Cons l { | subrecord } record_ record

  -- Recursively flatten subrecord
  , Eval ((Map ToRowListStep <<< FromRow) subrecord) subrowListOfRowLists
  , HFoldlWithIndex (FlattenRecStep sep subrecord) {} (Proxy subrowListOfRowLists) { | subrecord' }

  -- Prefix subrecord fields with current label
  , Symbol.Append l sep prefix
  , HFoldlWithIndex
      (Record.Prefix.PrefixProps prefix)
      (Record.Builder.Builder {} {})
      { | subrecord' }
      (Record.Builder.Builder {} { | subrecord'' })

  -- Insert the new set of fields to the result
  , Row.Union subrecord'' acc acc'
  ) =>
  FoldingWithIndex
    (FlattenRecStep sep record)
    (Proxy l)
    ({ | acc })
    (Proxy (Proxy (RL.Cons _l _a _t)))
    ({ | acc' }) where

  foldingWithIndex (FlattenRecStep sep record) l acc _ = do
    let
      subrecord = Record.get l record
      step = FlattenRecStep sep subrecord
      subrecord' = hfoldlWithIndex step {} (Proxy :: Proxy subrowListOfRowLists)

      prefix = Proxy :: Proxy prefix

    Record.union (Record.Prefix.add prefix subrecord') acc

else instance
  ( IsSymbol l
  , Row.Cons l a record_ record
  , Row.Lacks l acc
  , Row.Cons l a acc acc'
  ) =>
  FoldingWithIndex
    (FlattenRecStep sep record)
    (Proxy l)
    { | acc }
    _a
    { | acc' } where
  foldingWithIndex (FlattenRecStep _ record) l acc _ = do
    let
      v = Record.get l record
    Record.insert l v acc

nested =
  { a1:
    { a2:
      { a3: 8
      , b3: {}
      , d3: "bar"
      }
    , b2:
      { a3: "foo"
      , c3: "bar"
      }
    }
  }

flattenRec :: forall r r' rlOfRl. Eval ((Map ToRowListStep <<< FromRow) r) rlOfRl => HFoldlWithIndex (FlattenRecStep "." r) {} (Proxy rlOfRl) { | r' } => { | r } -> { | r' }
flattenRec record = hfoldlWithIndex (FlattenRecStep (Proxy :: Proxy ".") record) {} (Proxy :: Proxy rlOfRl)
