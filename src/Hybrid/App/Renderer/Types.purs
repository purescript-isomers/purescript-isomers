module Hybrid.App.Renderer.Types where

import Hybrid.HTTP.Exchange (Exchange)

type Renderer req res doc = Exchange req res → doc

