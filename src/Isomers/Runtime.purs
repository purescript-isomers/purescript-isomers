module Isomers.Runtime where

foreign import data Node :: Type

foreign import data Browser :: Type

foreign import data Deno :: Type

foreign import caseRuntime :: forall a. (Node -> a) -> (Browser -> a) -> (Deno -> a) -> a -> a
