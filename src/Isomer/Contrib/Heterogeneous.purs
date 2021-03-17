module Isomer.Contrib.Heterogeneous where


import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class MappingWithIndex, ConstMapping, hmap, hmapWithIndex)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (MapWithIndex)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (RProxy)

-- | Versions of `HMap` and `HMapWithIndex` which result in a "closed"
-- | row in the case of `Variant`.
foreign import data MappingWithIndexExpr :: Type → Type -> Type -> TypeExpr

instance evalMappingIndex :: (MappingWithIndex fn l a b) => Eval (MappingWithIndexExpr fn l a) b

-- | A version of HMap which closes result record in the case of `Variant`
class HMap' f a b | f a -> b where
  hmap' :: f -> a -> b

instance hMapVariant' ∷
  ( HMap f (Variant v) (Variant v')
  , Eval ((ToRow <<< MapWithIndex (MappingWithIndexExpr (ConstMapping f)) <<< FromRow) (RProxy v)) (RProxy v')
  ) ⇒ HMap' f (Variant v) (Variant v') where
  hmap' f v = hmap f v
else instance fallbackHMap ∷ (HMap f v v') ⇒ HMap' f v v' where
  hmap' = hmap

class HMapWithIndex' f a b | f a -> b where
  hmapWithIndex' :: f -> a -> b

instance hMapVariantWithIndex' ∷
  ( HMapWithIndex f (Variant v) (Variant v')
  , Eval ((ToRow <<< MapWithIndex (MappingWithIndexExpr f) <<< FromRow) (RProxy v)) (RProxy v')
  ) ⇒ HMapWithIndex' f (Variant v) (Variant v') where
  hmapWithIndex' f v = hmapWithIndex f v
else instance fallbackHMapWithIndex ∷ (HMapWithIndex f v v') ⇒ HMapWithIndex' f v v' where
  hmapWithIndex' = hmapWithIndex
