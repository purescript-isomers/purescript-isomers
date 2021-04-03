module Isomers.Web.Spec.Type where

-- | XXX: New Spec migration
import Data.Newtype (class Newtype)
import Isomers.Contrib.Heterogeneous.HEval ((<<<), type (<<<)) as H
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Type.Prelude (SProxy(..))

newtype Spec api renderers
  = Spec
  { api ∷ api
  , renderers ∷ renderers
  }

derive instance newtypeSpec ∷ Newtype (Spec api r) _

_api = SProxy ∷ SProxy "api"

type GetApi
  = Mappings.Record.Get "api" H.<<< Mappings.Newtype.Unwrap

_GetApi ∷ GetApi
_GetApi = Mappings.Record.Get _api H.<<< Mappings.Newtype.Unwrap

_renderers = SProxy ∷ SProxy "renderers"

type GetRenderers
  = Mappings.Record.Get "renderers" H.<<< Mappings.Newtype.Unwrap

_GetRenderers ∷ GetRenderers
_GetRenderers = Mappings.Record.Get _renderers H.<<< Mappings.Newtype.Unwrap

