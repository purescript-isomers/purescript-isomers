module Isomers.Node
  ( module Exports
  , root
  , spec
  )
  where

import Isomers.Node.Types (Body, Root, Spec)
import Isomers.Node.Types (Body, Root, Spec) as Exports
import Isomers.Spec (class Builder, root, spec) as Spec

spec ∷ ∀ a i req res. Spec.Builder a Body i req res ⇒ a → Spec i req res
spec = Spec.spec

root ∷ ∀ a req res. Spec.Builder a Body {} { | req } res ⇒ a → Root { | req } res
root = Spec.root
