module Isomers.Web.Types where

import Data.Newtype (class Newtype)
import Isomers.Contrib.Heterogeneous.HEval ((<<<), type (<<<)) as H
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomers.Spec (AccumSpec, Spec)
import Isomers.Spec (rootAccumSpec) as Spec
import Type.Prelude (Proxy(..))

newtype AccumWebSpec body rnd route ireq oreq res = AccumWebSpec
  { spec ∷ AccumSpec body route ireq oreq res
  , render ∷ rnd
  }

derive instance newtypeAccumWebSpec ∷ Newtype (AccumWebSpec body rnd route ireq oreq res) _

newtype WebSpec body rnd ireq oreq res = WebSpec
  { spec ∷ Spec body ireq oreq res
  , render ∷ rnd
  }

derive instance newtypeWeb ∷ Newtype (WebSpec body rnd ireq oreq res) _

type Web' body rnd req = WebSpec body rnd req req

-- | Heterogeneous mappings which extract pieces of the specs.
-- | Useful when you want to turn a record of specs into a record
-- | of its subparts.
_spec = Proxy ∷ Proxy "spec"

type GetSpec = Mappings.Record.Get "spec" H.<<< Mappings.Newtype.Unwrap

_GetSpec ∷ GetSpec
_GetSpec = Mappings.Record.Get _spec H.<<< Mappings.Newtype.Unwrap

_render = Proxy ∷ Proxy "render"

type GetRender = Mappings.Record.Get "render" H.<<< Mappings.Newtype.Unwrap

_GetRender ∷ GetRender
_GetRender = Mappings.Record.Get _render H.<<< Mappings.Newtype.Unwrap

rootAccumWebSpec ∷
  ∀ body ireq oreq res rnd.
  AccumWebSpec body rnd {} ireq oreq res →
  WebSpec body rnd ireq oreq res
rootAccumWebSpec (AccumWebSpec { render, spec }) = WebSpec
  { spec: Spec.rootAccumSpec spec
  , render
  }

