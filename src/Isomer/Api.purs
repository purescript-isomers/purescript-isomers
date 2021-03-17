module Isomer.Api
  ( client
  , module Client
  , module Spec
  )
  where

import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomer.Api.Client (ClientFolding, RequestBuildersFolding, client) as C
import Isomer.Api.Client (requestBuilders) as Client
import Isomer.Api.Spec (Spec(..))
import Isomer.Api.Spec (Spec(..), SpecFolding(..)) as Spec
import Type.Prelude (Proxy)

client ∷
  ∀ client requestBuilders responseDuplexes request.
  HFoldlWithIndex (C.RequestBuildersFolding request request) {} (Proxy request) { | requestBuilders } ⇒
  HFoldlWithIndex (C.ClientFolding request responseDuplexes) {} { | requestBuilders } { | client } ⇒
  Spec request responseDuplexes →
  { | client }
client (Spec { request, response }) = C.client request response

