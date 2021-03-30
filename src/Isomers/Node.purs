module Isomers.Node where

import Isomers.Node.Request.Body (Str, Buff) as Body
import Isomers.Spec (class Builder, Spec, spec) as Spec
import Type.Row (type (+))

type Body = (Body.Buff + Body.Str + ())

type Spec i req res = Spec.Spec Body i req res

type Root payload res = Spec {} { | payload } res

spec ∷ ∀ a i req res. Spec.Builder a Body i req res ⇒ a → Spec i req res
spec = Spec.spec

-- | TODO: root slash handling
root ∷ ∀ a req res. Spec.Builder a Body {} { | req } res ⇒ a → Root req res
root = Spec.spec
