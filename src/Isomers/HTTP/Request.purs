module Isomers.HTTP.Request
  ( module Method
  ) where

import Data.Variant (SProxy, Variant)
import Data.Variant (inj) as Variant
import Isomers.HTTP.Method (Method(..)) as Method
import Isomers.HTTP.Method (_get, _post)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)

newtype Request (method ∷ Symbol) req = Request (Variant req)

get :: forall l m req req' req_'. Row.Cons l (Variant ( "GET" :: req | m )) req_' req' => IsSymbol l => SProxy l -> req -> Request "GET" req'
get l req = Request (Variant.inj l (Variant.inj _get req))

post :: forall l m req req' req_'. Row.Cons l (Variant ( "POST" :: req | m )) req_' req' => IsSymbol l => SProxy l -> req -> Request "POST" req'
post l req = Request (Variant.inj l (Variant.inj _post req))
