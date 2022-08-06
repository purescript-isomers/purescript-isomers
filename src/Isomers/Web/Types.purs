module Isomers.Web.Types where

import Data.Newtype (class Newtype)
import Isomers.Contrib.Heterogeneous.HEval (type (<<<), (<<<)) as H
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomers.Spec (AccumSpec, Spec)
import Isomers.Spec (rootAccumSpec) as Spec
import Type.Prelude (Proxy(..))

newtype AccumWebSpec rnd route ireq oreq res = AccumWebSpec
  { spec :: AccumSpec route ireq oreq res
  , render :: rnd
  }

derive instance newtypeAccumWebSpec :: Newtype (AccumWebSpec rnd route ireq oreq res) _

newtype WebSpec rnd ireq oreq res = WebSpec
  { spec :: Spec ireq oreq res
  , render :: rnd
  }

derive instance newtypeWeb :: Newtype (WebSpec rnd ireq oreq res) _

type Web' rnd req = WebSpec rnd req req

-- | Heterogeneous mappings which extract pieces of the specs.
-- | Useful when you want to turn a record of specs into a record
-- | of its subparts.
_spec = Proxy :: Proxy "spec"

type GetSpec = Mappings.Record.Get "spec" H.<<< Mappings.Newtype.Unwrap

_GetSpec :: GetSpec
_GetSpec = Mappings.Record.Get _spec H.<<< Mappings.Newtype.Unwrap

_render = Proxy :: Proxy "render"

type GetRender = Mappings.Record.Get "render" H.<<< Mappings.Newtype.Unwrap

_GetRender :: GetRender
_GetRender = Mappings.Record.Get _render H.<<< Mappings.Newtype.Unwrap

rootAccumWebSpec
  :: forall ireq oreq res rnd
   . AccumWebSpec rnd {} ireq oreq res
  -> WebSpec rnd ireq oreq res
rootAccumWebSpec (AccumWebSpec { render, spec }) = WebSpec
  { spec: Spec.rootAccumSpec spec
  , render
  }

