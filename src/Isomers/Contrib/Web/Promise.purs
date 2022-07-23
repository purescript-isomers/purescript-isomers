module Isomers.Contrib.Web.Promise where

import Prelude

import Control.Promise (Promise, toAff, toAffE) as Control.Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)
import Web.Promise (Promise) as Web.Promise

toAffPromise :: forall a. Web.Promise.Promise a -> Control.Promise.Promise a
toAffPromise = unsafeCoerce

toAff :: forall t17. Web.Promise.Promise t17 -> Aff t17
toAff = Control.Promise.toAff <<< toAffPromise

toAffE :: forall t7. Effect (Web.Promise.Promise t7) -> Aff t7
toAffE = Control.Promise.toAffE <<< map toAffPromise

