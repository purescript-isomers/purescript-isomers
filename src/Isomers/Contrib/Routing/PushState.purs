module Isomers.Contrib.Routing.PushState where

import Prelude

import Data.Foldable (class Foldable, indexl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write) as Ref
import Routing.PushState (LocationState) as PushState
import Routing.PushState (PushStateInterface)

foldLocationsAff
  :: forall a
   . (a -> PushState.LocationState -> Aff a)
  -> a
  -> PushStateInterface
  -> Effect (Effect Unit)
foldLocationsAff cb init psi = do
  ref <- Ref.new init
  let
    handle loc = launchAff_ do
      prev <- liftEffect $ Ref.read ref
      new <- cb prev loc
      liftEffect $ Ref.write new ref
  handle =<< psi.locationState
  psi.listen handle

foldPathsAff
  :: forall a
   . (a -> String -> Aff a)
  -> a
  -> PushStateInterface
  -> Effect (Effect Unit)
foldPathsAff cb init = foldLocationsAff (\a -> cb a <<< _.path) init

matchesWithAff
  :: forall f a
   . Foldable f
  => (String -> Aff (f a))
  -> (Maybe a -> a -> Effect Unit)
  -> PushStateInterface
  -> Effect (Effect Unit)
matchesWithAff parser cb = foldPathsAff go Nothing
  where
  cb' a = case _ of
    Just b -> do
      liftEffect $ cb a b
      pure $ Just b
    Nothing -> pure a
  go a =
    cb' a
      <=< pure <<< indexl 0
      <=< parser

