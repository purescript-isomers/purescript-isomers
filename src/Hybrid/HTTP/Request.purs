module Hybrid.HTTP.Request
  ( module Data
  , module Method
  ) where

import Data.Variant (SProxy, Variant)
import Data.Variant (inj) as Variant
import Hybrid.HTTP.Request.Data (Data(..)) as Data
import Hybrid.HTTP.Request.Method (_get)
import Hybrid.HTTP.Request.Method (Method(..)) as Method
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)

newtype Request (method ∷ Symbol) req
  = Request (Variant req)

get :: forall l m req req' req_'. Row.Cons l (Variant ( "GET" :: req | m )) req_' req' => IsSymbol l => SProxy l -> req -> Request "GET" req'
get l req = Request (Variant.inj l (Variant.inj _get req))
