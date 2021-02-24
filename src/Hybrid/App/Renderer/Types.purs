module Hybrid.App.Renderer.Types where

import Data.Maybe (Maybe)
import Data.Either.Nested (type (\/))
import Hybrid.Api.Spec (FetchError)
import Hybrid.Response (Response)

type Renderer req res doc
  = req → Maybe (FetchError \/ Response res) → doc

