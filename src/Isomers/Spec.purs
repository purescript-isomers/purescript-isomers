module Isomers.Spec (client, module Exports, requestBuilders) where

import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (ClientStep, RequestBuildersStep, client, requestBuilders) as Client
import Isomers.Client.Fetch (HostInfo)
import Isomers.Spec.Builder (spec, accumSpec, Scalar(..), Insert(..), class Builder, BuilderStep(..)) as Exports
import Isomers.Spec.Types (Spec(..))
import Isomers.Spec.Types (rootAccumSpec, AccumSpec(..), Spec(..)) as Exports
import Type.Prelude (Proxy(..))

requestBuilders ∷
  ∀ body ireq oreq requestBuilders response.
  HFoldlWithIndex (Client.RequestBuildersStep ireq ireq) {} (Proxy ireq) { | requestBuilders } ⇒
  Spec body ireq oreq response →
  { | requestBuilders }
requestBuilders (Spec _) = Client.requestBuilders (Proxy ∷ Proxy ireq)

client ∷
  ∀ body client requestBuilders responseDuplexes ireq oreq.
  HFoldlWithIndex (Client.RequestBuildersStep ireq ireq) {} (Proxy ireq) requestBuilders ⇒
  HFoldlWithIndex (Client.ClientStep ireq responseDuplexes) {} requestBuilders client ⇒
  HostInfo →
  Spec body ireq oreq responseDuplexes →
  client
client hostInfo (Spec r) = Client.client hostInfo r.request r.response
