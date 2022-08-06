module Isomers.Spec.Types.Mappings where

import Isomers.Contrib.Heterogeneous.HEval (type (<<<), (<<<)) as H
import Isomers.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomers.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Type.Prelude (Proxy(..))

-- Reach out for "request" and "response" fields from a newtyped record

_request = Proxy :: Proxy "request"

_response = Proxy :: Proxy "response"

type GetRequest = Mappings.Record.Get "request" H.<<< Mappings.Newtype.Unwrap

_GetRequest :: GetRequest
_GetRequest = Mappings.Record.Get _request H.<<< Mappings.Newtype.Unwrap

type GetResponse = Mappings.Record.Get "response" H.<<< Mappings.Newtype.Unwrap

_GetResponse :: GetResponse
_GetResponse = Mappings.Record.Get _response H.<<< Mappings.Newtype.Unwrap

