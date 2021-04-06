module Isomers.Node
  ( module Exports
  , root
  , spec
  )
  where

import Isomers.Node.Types (Body, Root, Spec)
import Isomers.Node.Types (Body, Root, Spec) as Exports
import Isomers.Spec (class Builder, root, spec) as Spec
import Isomers.Spec.Builder (SpecStep(..))

spec ∷ ∀ a ireq oreq route res. Spec.Builder a Body route ireq oreq res ⇒ a → Spec route ireq oreq res
spec = Spec.spec SpecStep

root ∷ ∀ a ireq oreq res. Spec.Builder a Body {} { | ireq } { | oreq } res ⇒ a → Root { | ireq } { | oreq } res
root = Spec.root
