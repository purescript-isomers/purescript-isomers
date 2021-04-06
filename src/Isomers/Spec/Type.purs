module Isomers.Spec.Type where


import Data.Newtype (class Newtype)
import Isomers.Contrib.Heterogeneous.HEval ((<<<), type (<<<)) as H
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomers.Request (Accum, Duplex') as Request
import Isomers.Request.Accum (insert) as Accum
import Isomers.Request.Accum (unifyRoute) as Request.Accum
import Prim.Row (class Cons, class Lacks) as Row
import Type.Equality (class TypeEquals)
import Type.Prelude (class IsSymbol, SProxy(..))

-- | We keep ireq polymorphic during the composition and this
-- | does the magic so composition align
newtype Spec body route ireq oreq res
  = Spec
  { request ∷ Request.Accum body route ireq oreq
  , response ∷ res
  }

derive instance newtypeSpec ∷ Newtype (Spec req route ireq oreq res) _

_request = SProxy ∷ SProxy "request"

_response = SProxy ∷ SProxy "response"

type RequestMapping
  = Mappings.Record.Get "request" H.<<< Mappings.Newtype.Unwrap

_RequestMapping ∷ RequestMapping
_RequestMapping = Mappings.Record.Get _request H.<<< Mappings.Newtype.Unwrap

type ResponseMapping = Mappings.Record.Get "response" H.<<< Mappings.Newtype.Unwrap

_ResponseMapping ∷ ResponseMapping
_ResponseMapping = Mappings.Record.Get _response H.<<< Mappings.Newtype.Unwrap

insert ∷
  ∀ a body l ireq oreq route route' res.
  IsSymbol l ⇒
  Row.Lacks l route ⇒
  Row.Cons l a route route' ⇒
  SProxy l →
  Request.Duplex' body a →
  Spec body { | route' } ireq oreq res →
  Spec body { | route } ireq oreq res
insert l dpl (Spec { request, response }) = Spec
  { request: Accum.insert l dpl request
  , response
  }

unifyRoute ∷ ∀ body ireq oreq res route route'. TypeEquals route route' ⇒ Spec body route ireq oreq res → Spec body route' ireq oreq res
unifyRoute (Spec { request, response }) = Spec { request: Request.Accum.unifyRoute request, response }
