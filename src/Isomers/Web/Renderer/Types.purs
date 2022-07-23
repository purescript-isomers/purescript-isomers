module Isomers.Web.Renderer.Types where

import Prelude

import Data.Tuple.Nested (type (/\))
import Isomers.HTTP.Exchange (Exchange)

-- | TODO: Drop `Exchange` from here?
newtype Renderer router req res doc = Renderer ((router /\ Exchange req res) -> doc)

derive instance functorRenderer :: Functor (Renderer router req res)
