module Isomers.Contrib.Heterogeneous.Foldings where

import Prelude

import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.Symbol (class Append) as Symbol
import Record (insert, union) as Record
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Type.Prelude (class IsSymbol, Proxy(..), SProxy(SProxy))

newtype Flatten (sep ∷ Symbol)
  = Flatten (SProxy sep)

-- | TODO: Cover `Variant` too.
instance flattenRecordRec ∷
  ( HFoldlWithIndex (Flatten sep) {} { | res } { | res' }
  , HFoldlWithIndex (Record.Prefix.PrefixProps sym) (Record.Builder.Builder {} {}) { | res' } (Record.Builder.Builder {} { | res'' })
  , Symbol.Append l sep sym
  , Row.Union acc res'' acc'
  ) ⇒
  FoldingWithIndex
    (Flatten sep)
    (SProxy l)
    ({ | acc })
    ({ | res })
    ({ | acc' }) where
  foldingWithIndex f@(Flatten sep) l acc v = do
    let
      sym = SProxy ∷ SProxy sym
      res' ∷ { | res' }
      res' = hfoldlWithIndex f {} v
    Record.union acc (Record.Prefix.add sym res')
else instance flattenRecord ∷
  ( IsSymbol l
  , Row.Lacks l acc
  , Row.Cons l a acc acc'
  ) ⇒
  FoldingWithIndex
    (Flatten sep)
    (SProxy l)
    { | acc }
    a
    { | acc' } where
  foldingWithIndex pref l acc v = Record.insert l v acc

class Intercalate (sep ∷ Symbol) (pref ∷ Symbol) (curr ∷ Symbol) (result ∷ Symbol) | sep pref curr → result
instance intercalateEmpty ∷ Intercalate sep "" curr curr
else instance intercalateNoneEmpty ∷
  ( Symbol.Append pref sep pref'
  , Symbol.Append pref' curr res
  ) =>
  Intercalate sep pref curr res

-- 
-- newtype FlattenVariant (sep ∷ Symbol) (path ∷ Symbol)
--   = FlattenVariant (SProxy sep)
-- 
-- instance flattenVariantRec ∷
--   ( Intercalate sep path sym path'
--   , HFoldlWithIndex (FlattenVariant sep path') {} (Proxy (Variant sub)) { | flatten }
--   , 
--   ) ⇒
--   FoldingWithIndex
--     (Flatten sep path)
--     (SProxy sym)
--     (sv → { | flatten })
--     (Proxy (Variant sub))
--     (sv' → { | flatten' })
-- 
-- instance flattenVariantValue ∷
--   ( Row.Cons sym (a → a) flatten flatten'
--   , Row.Lacks sym flatten
--   , IsSymbol sym
--   ) ⇒
--   FoldingWithIndex
--     (FlattenVariant sep path')
--     (SProxy sym)
--     { | flatten }
--     (Proxy a)
--     { | flatten' } where
--   foldingWithIndex _ sym flatten _ = Record.insert sym identity flatten
--   
