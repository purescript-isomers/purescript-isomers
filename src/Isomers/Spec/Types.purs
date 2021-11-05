module Isomers.Spec.Types where

import Data.Newtype (class Newtype)
import Isomers.Contrib.Heterogeneous.HEval ((<<<), type (<<<)) as H
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomers.Request (Accum, Duplex) as Request
import Isomers.Request.Accum (rootDuplex, unifyRoute) as Request.Accum
import Type.Equality (class TypeEquals)
import Type.Prelude (Proxy(..))

-- | TODO: Is changing the order and moving `ireq` and `oreq`
-- | to the end worth it? We can provide __probably__ more useful
-- | instances then....
newtype AccumSpec (body ∷ # Type) route ireq oreq res
  = AccumSpec
  { request ∷ Request.Accum body route ireq oreq
  , response ∷ res
  }

derive instance newtypeAccumSpec ∷ Newtype (AccumSpec req route ireq oreq res) _

_request = Proxy ∷ Proxy "request"

_response = Proxy ∷ Proxy "response"

type GetRequest
  = Mappings.Record.Get "request" H.<<< Mappings.Newtype.Unwrap

_GetRequest ∷ GetRequest
_GetRequest = Mappings.Record.Get _request H.<<< Mappings.Newtype.Unwrap

type GetResponse = Mappings.Record.Get "response" H.<<< Mappings.Newtype.Unwrap

_GetResponse ∷ GetResponse
_GetResponse = Mappings.Record.Get _response H.<<< Mappings.Newtype.Unwrap

unifyRoute ∷ ∀ body ireq oreq res route route'. TypeEquals route route' ⇒ AccumSpec body route ireq oreq res → AccumSpec body route' ireq oreq res
unifyRoute (AccumSpec { request, response }) = AccumSpec { request: Request.Accum.unifyRoute request, response }


newtype Spec body ireq oreq res
  = Spec
  { request ∷ Request.Duplex body ireq oreq
  , response ∷ res
  }

type Spec' body req res = Spec body req req res

derive instance newtypeSpec ∷ Newtype (Spec req ireq oreq res) _

-- | Let's make an initial unit an empty record `{}` for now.
-- | This is consistent with combinators like `hnil`, `scalar` etc.
-- | from `Request.Accum`.
rootAccumSpec ∷ ∀ body ireq oreq res. AccumSpec body {} ireq oreq res → Spec body ireq oreq res
rootAccumSpec (AccumSpec { request: accum, response }) = Spec
  { request: Request.Accum.rootDuplex accum
  , response
  }

