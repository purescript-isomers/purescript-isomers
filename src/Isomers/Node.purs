module Isomers.Node where

import Isomers.Node.Request.Body (Str, Buff) as Body
import Isomers.Spec (Spec)
import Isomers.Spec (class Builder, spec) as Spec
import Type.Row (type (+))

type Body = (Body.Buff + Body.Str + ())

spec ∷ ∀ a i req res. Spec.Builder a Body i req res ⇒ a → Spec Body i req res
spec = Spec.spec
