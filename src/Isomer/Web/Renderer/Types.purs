module Isomer.Web.Renderer.Types where

import Data.Tuple.Nested (type (/\))
import Isomer.HTTP.Exchange (Exchange)

-- | TODO: Drop `Exchange` from here?
type Renderer router req res content doc = (router /\ Exchange res req content) → doc
