module Hybrid.Contrib.Request.Duplex where

import Prelude
import Data.Either (Either(..))
import Request.Duplex (RequestDuplex', as)

unitDuplex ∷ RequestDuplex' String → RequestDuplex' Unit
unitDuplex = as printer parser
  where
  printer _ = ""

  parser _ = Right unit
