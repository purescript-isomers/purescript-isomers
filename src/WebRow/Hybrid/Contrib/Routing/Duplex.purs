module WebRow.Hybrid.Contrib.Routing.Duplex where


import Prelude

import Data.Either (Either(..))
import Routing.Duplex (RouteDuplex', as)

unitDuplex ∷ RouteDuplex' String → RouteDuplex' Unit
unitDuplex = as printer parser
  where
    printer _ = ""
    parser _ = Right unit
