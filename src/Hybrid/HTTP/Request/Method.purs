module Hybrid.HTTP.Request.Method where

import Prelude

import Data.Functor.Variant (SProxy(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, mapping)
import Hybrid.Contrib.Heterogeneous (class HMap', class HMapWithIndex', hmap', hmapWithIndex')
import Type.Equality (to) as Type.Equality
import Type.Prelude (class TypeEquals)
import Type.Row (type (+))

newtype Method (row ∷ #Type) = Method (Variant row)
instance newtypeMethod ∷ Newtype (Method row) (Variant row) where
  wrap v = Method v
  unwrap (Method v) = v

instance hmapMethod ∷ (HMap' f (Variant v) (Variant v')) ⇒
  HMap f (Method v) (Method v') where
  hmap f (Method v) = Method (hmap' f v)

instance foldlMethod ∷ (HFoldl f acc (Variant v) a) ⇒ HFoldl f acc (Method v) a where
  hfoldl f acc (Method v) = hfoldl f acc v

type DELETE req methods = ("DELETE" ∷ req | methods)
type GET req methods = ("GET" ∷ req | methods)
type POST req methods = ("POST" ∷ req | methods)
type PUT req methods = ("PUT" ∷ req | methods)

_delete = SProxy ∷ SProxy "DELETE"

_get = SProxy ∷ SProxy "GET"

_post = SProxy ∷ SProxy "POST"

_put = SProxy ∷ SProxy "PUT"


delete ∷ ∀ methods req. req → Method (DELETE req + methods)
delete = Method <<< inj _delete

get ∷ ∀ methods req. req → Method (GET req + methods)
get = Method <<< inj _get

post ∷ ∀ methods req. req → Method (POST req + methods)
post = Method <<< inj _post

put ∷ ∀ methods req. req → Method (PUT req + methods)
put = Method <<< inj _put



-- | This can be a bit confusing but we have three mappings for
-- | `Method` defined here.
-- | The first one is going to apply the innter mapping on the content
-- | of methods.
-- newtype MethodMapping f = MethodMapping f
-- 
-- instance mappingMethod ∷
--   (HMap' f (Variant v) (Variant v'))
--   ⇒ Mapping (MethodMapping f) (Method v) (Method v') where
--   mapping (MethodMapping f) (Method a) = Method (hmap' f a)
-- -- else instance recMappingMethod
-- --   ∷ (Mapping (MethodMapping f) v v')
-- --   ⇒ Mapping (MethodMapping f) v v' where
-- --   mapping mm v = mapping mm v
-- 
-- newtype MethodMappingWithIndex f = MethodMappingWithIndex f
-- 
-- instance mappingWithIndexMethod ∷
--   (HMapWithIndex' f (Variant v) (Variant v'))
--   ⇒ MappingWithIndex (MethodMappingWithIndex f) l (Method v) (Method v') where
--   mappingWithIndex (MethodMappingWithIndex f) l (Method a) = Method (hmapWithIndex' f a)
-- -- else instance recMappingWithIndexMethod
-- --   ∷ ( HMapWithIndex' (MethodMappingWithIndex f) v v')
-- --   ⇒ MappingWithIndex (MethodMappingWithIndex f) l v v' where
-- --   mappingWithIndex mm l v = hmapWithIndex' mm v
-- 
-- newtype MethodApply f = MethodApply f
-- 
-- -- | This mapping is called when `Method` is a part of inner row
-- -- | which we map over.
-- instance methodApply ∷
--   (TypeEquals f (Variant v → Variant v')) ⇒
--   Mapping (MethodApply f) (Method v) (Method v') where
--   mapping (MethodApply f) (Method v) = Method (Type.Equality.to f v)
-- else instance recMethodApply
--   ∷ (HMap' (MethodApply f) v v')
--   ⇒ Mapping (MethodApply f) v v' where
--   mapping mm v = hmap' mm v
-- 
-- -- | When particular methods are applied to `Method` we want
-- -- | want to apply mapping directly on the inner `Variant`.
-- instance hmapMethodApply ∷
--   (TypeEquals f ((Variant v) → (Variant v'))) ⇒
--   HMap (MethodApply f) (Method v) (Method v') where
--   hmap (MethodApply f) (Method v) = Method (Type.Equality.to f v)
-- else instance hmapMethodMapping ∷
--   (HMap' f (Variant v) (Variant v')) ⇒
--   HMap (MethodMapping f) (Method v) (Method v') where
--   hmap (MethodMapping f) (Method v) = Method (hmap' f v)
--
--
-- instance hmapWithIndexMethodMappingWithIndex ∷
--   (HMapWithIndex' f (Variant v) (Variant v')) ⇒
--   HMapWithIndex (MethodMappingWithIndex f) (Method v) (Method v') where
--   hmapWithIndex (MethodMappingWithIndex f) (Method v) = Method (hmapWithIndex' f v)
-- else instance hmapWithIndexMethod ∷
--   ( HMapWithIndex' f (Variant v) (Variant v')) ⇒
--   HMapWithIndex f (Method v) (Method v') where
--   hmapWithIndex f (Method v) = Method (hmapWithIndex' f v)
