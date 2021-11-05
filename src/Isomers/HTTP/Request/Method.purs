module Isomers.HTTP.Request.Method where

import Prelude
import Control.Comonad (class Comonad, class Extend)
import Data.HTTP.Method (Method(..)) as HTTP.Method
import Data.HTTP.Method (Method) as HTTP
import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Data.Variant (case_, on) as Variant
import Heterogeneous.Folding (class HFoldl, class HFoldlWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex)
import Isomers.Contrib.Heterogeneous (class HMap', class HMapWithIndex', hmap', hmapWithIndex')
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

newtype Method m
  = Method m

derive instance newtypeMethod ∷ Newtype (Method m) _

derive instance functorMethod ∷ Functor Method

instance hmapMethod ∷ (HMap' f v v') ⇒ HMap f (Method v) (Method v') where
  hmap f (Method v) = Method (hmap' f v)

instance hmapWithIndexMethod ∷ (HMapWithIndex' f v v') ⇒ HMapWithIndex f (Method v) (Method v') where
  hmapWithIndex f (Method v) = Method (hmapWithIndex' f v)

instance hfoldlMethod ∷ (HFoldl f acc v a) ⇒ HFoldl f acc (Method v) a where
  hfoldl f acc (Method v) = hfoldl f acc v

instance hfoldlWithIndexMethod ∷ (HFoldlWithIndex f acc v a) ⇒ HFoldlWithIndex f acc (Method v) a where
  hfoldlWithIndex f acc (Method v) = hfoldlWithIndex f acc v

instance extendIdentity :: Extend Method where
  extend f m = Method (f m)

instance comonadIdentity :: Comonad Method where
  extract (Method x) = x

type DELETE a methods
  = ( "DELETE" ∷ a | methods )

type GET a methods
  = ( "GET" ∷ a | methods )

type POST a methods
  = ( "POST" ∷ a | methods )

type PUT a methods
  = ( "PUT" ∷ a | methods )

_delete = Proxy ∷ Proxy "DELETE"

_get = Proxy ∷ Proxy "GET"

_post = Proxy ∷ Proxy "POST"

_put = Proxy ∷ Proxy "PUT"

delete ∷ ∀ methods req. req → Method (Variant (DELETE req + methods))
delete = Method <<< inj _delete

get ∷ ∀ methods req. req → Method (Variant (GET req + methods))
get = Method <<< inj _get

post ∷ ∀ methods req. req → Method (Variant (POST req + methods))
post = Method <<< inj _post

put ∷ ∀ methods req. req → Method (Variant (PUT req + methods))
put = Method <<< inj _put

toHTTPMethod ::
  forall g t103 t85 t94.
  Method
    ( Variant
        ( "DELETE" :: t85
        , "GET" ∷ g
        , "POST" :: t103
        , "PUT" :: t94
        )
    ) ->
  HTTP.Method
toHTTPMethod (Method v) = convert v
  where
  convert =
    Variant.case_
      # Variant.on _delete (const $ HTTP.Method.DELETE)
      # Variant.on _put (const $ HTTP.Method.PUT)
      # Variant.on _post (const $ HTTP.Method.POST)
      # Variant.on _get (const $ HTTP.Method.GET)

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
