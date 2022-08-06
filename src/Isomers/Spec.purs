module Isomers.Spec (client, module Exports, requestBuilders) where

import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (ClientStep, RequestBuildersStep, client, requestBuilders) as Client
import Isomers.Client (Fetch)
import Isomers.Spec.FoldSpec (class FoldSpec, FoldSpecStep(..), Insert(..), Scalar(..), WithBody(..), accumSpec, foldSpec) as Exports
import Isomers.Spec.Types (AccumSpec(..), Spec(..), rootAccumSpec) as Exports
import Isomers.Spec.Types (Spec(..))
import Type.Prelude (Proxy(..))

requestBuilders
  :: forall ireq oreq requestBuilders response
   . HFoldlWithIndex (Client.RequestBuildersStep ireq ireq) {} (Proxy ireq) { | requestBuilders }
  => Spec ireq oreq response
  -> { | requestBuilders }
requestBuilders (Spec _) = Client.requestBuilders (Proxy :: Proxy ireq)

client
  :: forall client requestBuilders responseDuplexes ireq oreq
   . HFoldlWithIndex (Client.RequestBuildersStep ireq ireq) {} (Proxy ireq) { | requestBuilders }
  => HFoldlWithIndex (Client.ClientStep ireq responseDuplexes) {} { | requestBuilders } client
  => Fetch
  -> Spec ireq oreq responseDuplexes
  -> client
client fetch (Spec r) = Client.client fetch r.request r.response
