module Isomers.HTTP.Request.Accept where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, mapping)
import Isomers.Contrib.Heterogeneous (class HMap', class HMapWithIndex', hmap', hmapWithIndex')
import Isomers.HTTP.ContentTypes (Jpeg, Png, Svg)
import Isomers.HTTP.ContentTypes (Json, jpeg, json, png, svg) as ContentTypes
import Type.Equality (to) as Type.Equality
import Type.Prelude (class TypeEquals)
import Type.Row (type (+))

newtype Accept v = Accept v
derive instance newtypeAccept ∷ Newtype (Accept v) _

-- instance hmapAccept ∷ (HMap' f (Variant v) (Variant v')) ⇒
--   HMap f (Accept v) (Accept v') where
--   hmap f (Accept v) = Accept (hmap' f v)
-- 
-- instance foldlAccept ∷ (HFoldl f acc (Variant v) a) ⇒ HFoldl f acc (Accept v) a where
--   hfoldl f acc (Accept v) = hfoldl f acc v

jpeg ∷ ∀ a cts. a → Accept (Variant (Jpeg a + cts))
jpeg = Accept <<< ContentTypes.jpeg

json ∷ ∀ a cts. a → Accept (Variant (ContentTypes.Json a + cts))
json = Accept <<< ContentTypes.json

png ∷ ∀ a cts. a → Accept (Variant (Png a + cts))
png = Accept <<< ContentTypes.png

svg ∷ ∀ a cts. a → Accept (Variant (Svg a + cts))
svg = Accept <<< ContentTypes.svg


-- 
-- 
-- -- | This can be a bit confusing but we have three mappings for
-- -- | `Accept` defined here.
-- -- | The first one is going to apply the innter mapping on the content
-- -- | of accepts.
-- newtype AcceptMapping f = AcceptMapping f
-- 
-- instance mappingAccept ∷
--   (HMap' f (Variant v) (Variant v'))
--   ⇒ Mapping (AcceptMapping f) (Accept v) (Accept v') where
--   mapping (AcceptMapping f) (Accept a) = Accept (hmap' f a)
-- else instance recMappingAccept
--   ∷ (Mapping (AcceptMapping f) v v')
--   ⇒ Mapping (AcceptMapping f) v v' where
--   mapping mm v = mapping mm v
-- 
-- newtype AcceptMappingWithIndex f = AcceptMappingWithIndex f
-- 
-- instance mappingWithIndexAccept ∷
--   (HMapWithIndex' f (Variant v) (Variant v'))
--   ⇒ MappingWithIndex (AcceptMappingWithIndex f) l (Accept v) (Accept v') where
--   mappingWithIndex (AcceptMappingWithIndex f) l (Accept a) = Accept (hmapWithIndex' f a)
-- else instance recMappingWithIndexAccept
--   ∷ ( HMapWithIndex' (AcceptMappingWithIndex f) v v')
--   ⇒ MappingWithIndex (AcceptMappingWithIndex f) l v v' where
--   mappingWithIndex mm l v = hmapWithIndex' mm v
-- 
-- newtype AcceptApply f = AcceptApply f
-- 
-- -- | This mapping is called when `Accept` is a part of inner row
-- -- | which we map over.
-- instance acceptApply ∷
--   (TypeEquals f (Variant v → Variant v')) ⇒
--   Mapping (AcceptApply f) (Accept v) (Accept v') where
--   mapping (AcceptApply f) (Accept v) = Accept (Type.Equality.to f v)
-- else instance recAcceptApply
--   ∷ (HMap' (AcceptApply f) v v')
--   ⇒ Mapping (AcceptApply f) v v' where
--   mapping mm v = hmap' mm v
-- 
-- -- | When particular accepts are applied to `Accept` we want
-- -- | want to apply mapping directly on the inner `Variant`.
-- instance hmapAcceptApply ∷
--   (TypeEquals f ((Variant v) → (Variant v'))) ⇒
--   HMap (AcceptApply f) (Accept v) (Accept v') where
--   hmap (AcceptApply f) (Accept v) = Accept (Type.Equality.to f v)
-- else instance hmapAcceptMapping ∷
--   (HMap' f (Variant v) (Variant v')) ⇒
--   HMap (AcceptMapping f) (Accept v) (Accept v') where
--   hmap (AcceptMapping f) (Accept v) = Accept (hmap' f v)
-- else instance hmapAccept ∷
--   ( HMap' f (Variant v) (Variant v')) ⇒
--   HMap f (Accept v) (Accept v') where
--   hmap f (Accept v) = Accept (hmap' f v)
-- 
-- instance hmapWithIndexAcceptMappingWithIndex ∷
--   (HMapWithIndex' f (Variant v) (Variant v')) ⇒
--   HMapWithIndex (AcceptMappingWithIndex f) (Accept v) (Accept v') where
--   hmapWithIndex (AcceptMappingWithIndex f) (Accept v) = Accept (hmapWithIndex' f v)
-- else instance hmapWithIndexAccept ∷
--   ( HMapWithIndex' f (Variant v) (Variant v')) ⇒
--   HMapWithIndex f (Accept v) (Accept v') where
--   hmapWithIndex f (Accept v) = Accept (hmapWithIndex' f v)
