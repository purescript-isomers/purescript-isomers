module Hybrid.Contrib.Data.Functor.Indexed.Compose where

import Data.Newtype (class Newtype)

-- newtype IxFCompose idx i o f a = IxFCompose (idx i o (f a))
-- derive instance newtypeCompose ∷ Newtype (Compose idx i o f a) _
-- 
-- instance idexedfunctorCompose
