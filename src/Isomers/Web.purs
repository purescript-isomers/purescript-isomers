module Isomers.Web
  ( module Exports
  , requestBuilders
  , toSpec
  ) where

import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (RequestBuildersStep, requestBuilders) as Client
import Isomers.Spec (Spec)
import Isomers.Web.Builder (Rendered(..)) as Exports
import Isomers.Web.Types (WebSpec(..))
import Type.Prelude (Proxy(..))

-- import Isomers.Web.Types (WebSpec(..))

-- | XXX: New Spec migration
--   ( module Spec
--   ) where
-- 
-- import Isomers.Web.Spec (Spec(..)) as Spec

requestBuilders
  :: forall ireq oreq rnd requestBuilders response
   . HFoldlWithIndex (Client.RequestBuildersStep ireq ireq) {} (Proxy ireq) { | requestBuilders }
  => WebSpec rnd ireq oreq response
  -> { | requestBuilders }
requestBuilders (WebSpec _) = Client.requestBuilders (Proxy :: Proxy ireq)

toSpec :: forall t3 t4 t5 t6. WebSpec t6 t5 t4 t3 -> Spec t5 t4 t3
toSpec (WebSpec { spec }) = spec
