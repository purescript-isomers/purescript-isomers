module Isomers.Web
  ( module Exports
  , requestBuilders
  , toSpec
  )
  where

import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (RequestBuildersStep, requestBuilders) as Client
import Isomers.Spec (Spec(..))
import Isomers.Web.Flatten (flatten, flatten') as Exports
import Isomers.Web.Builder (Rendered(..)) as Exports
import Isomers.Web.Types (WebSpec(..))
import Type.Prelude (Proxy(..))

-- import Isomers.Web.Types (WebSpec(..))

-- | XXX: New Spec migration
--   ( module Spec
--   ) where
-- 
-- import Isomers.Web.Spec (Spec(..)) as Spec

requestBuilders ∷
  ∀ body ireq oreq rnd requestBuilders response.
  HFoldlWithIndex (Client.RequestBuildersStep ireq ireq) {} (Proxy ireq) { | requestBuilders } ⇒
  WebSpec body rnd ireq oreq response →
  { | requestBuilders }
requestBuilders (WebSpec _) = Client.requestBuilders (Proxy ∷ Proxy ireq)

toSpec :: forall t3 t4 t5 t6 t7. WebSpec t7 t6 t5 t4 t3 -> Spec t7 t5 t4 t3
toSpec (WebSpec { spec }) = spec
