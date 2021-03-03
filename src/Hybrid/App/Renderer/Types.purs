module Hybrid.App.Renderer.Types where

import Hybrid.HTTP.Exchange (Exchange)

-- | TODO: Drop `Exchange` from here?
type Renderer req res doc = Exchange req res → doc

