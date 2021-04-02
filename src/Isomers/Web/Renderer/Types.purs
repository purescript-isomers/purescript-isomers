module Isomers.Web.Renderer.Types where

import Data.Tuple.Nested (type (/\))
import Isomers.HTTP.Exchange (Exchange)

-- | TODO: Drop `Exchange` from here?
newtype Renderer router req res doc = Renderer ((router /\ Exchange req res) → doc)


