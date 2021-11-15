module Isomers.Contrib.Web.Router.Driver.PushState where

import Prelude

import Data.Foldable (class Foldable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Isomers.Contrib.Routing.PushState (matchesWithAff) as PushState
import Routing.PushState (PushStateInterface)
import Routing.PushState (makeInterface) as PushState
import Web.Router.Types (Driver(..))

makeDriverAff :: forall f i o. Foldable f => (String -> Aff (f i)) -> (o -> String) -> Effect (Driver i o)
makeDriverAff parser printer = do
  makeDriverAff_ parser printer <$> PushState.makeInterface

makeDriverAff_ :: forall f i o. Foldable f => (String -> Aff (f i)) -> (o -> String) -> PushStateInterface -> Driver i o
makeDriverAff_ parser printer interface =
  Driver
    { initialize: \k -> PushState.matchesWithAff parser (\_ -> k) interface
    , navigate: interface.pushState (unsafeToForeign {}) <<< printer
    , redirect: interface.replaceState (unsafeToForeign {}) <<< printer
    }
