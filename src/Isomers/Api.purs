module Isomers.Api
  ( client
  , module Client
  , module Spec
  )
  where

import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Api.Client (ClientFolding, RequestBuildersFolding, client) as C
import Isomers.Api.Client (requestBuilders) as Client
import Isomers.Api.Spec (Spec(..))
import Isomers.Api.Spec (Spec(..), SpecFolding(..)) as Spec
import Type.Prelude (Proxy)

client ∷
  ∀ client requestBuilders responseDuplexes request.
  HFoldlWithIndex (C.RequestBuildersFolding request request) {} (Proxy request) { | requestBuilders } ⇒
  HFoldlWithIndex (C.ClientFolding request responseDuplexes) {} { | requestBuilders } { | client } ⇒
  Spec request responseDuplexes →
  { | client }
client (Spec { request, response }) = C.client request response
