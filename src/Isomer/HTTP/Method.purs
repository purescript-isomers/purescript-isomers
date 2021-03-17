module Isomer.HTTP.Method where


import Data.Functor.Variant (SProxy(..))
import Data.Newtype (class Newtype)
import Heterogeneous.Folding (class HFoldl, class HFoldlWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex)
import Isomer.Contrib.Heterogeneous (class HMap', class HMapWithIndex', hmap', hmapWithIndex')

newtype Method m = Method m
derive instance newtypeMethod ∷ Newtype (Method m) _

instance hmapMethod ∷ (HMap' f v v') ⇒ HMap f (Method v) (Method v') where
  hmap f (Method v) = Method (hmap' f v)

instance hmapWithIndexMethod ∷ (HMapWithIndex' f v v') ⇒ HMapWithIndex f (Method v) (Method v') where
  hmapWithIndex f (Method v) = Method (hmapWithIndex' f v)

instance hfoldlMethod ∷ (HFoldl f acc v a) ⇒ HFoldl f acc (Method v) a where
  hfoldl f acc (Method v) = hfoldl f acc v

instance hfoldlWithIndexMethod ∷ (HFoldlWithIndex f acc v a) ⇒ HFoldlWithIndex f acc (Method v) a where
  hfoldlWithIndex f acc (Method v) = hfoldlWithIndex f acc v

type DELETE a methods = ("DELETE" ∷ a | methods)
type GET a methods = ("GET" ∷ a | methods)
type POST a methods = ("POST" ∷ a | methods)
type PUT a methods = ("PUT" ∷ a | methods)

_delete = SProxy ∷ SProxy "DELETE"

_get = SProxy ∷ SProxy "GET"

_post = SProxy ∷ SProxy "POST"

_put = SProxy ∷ SProxy "PUT"
