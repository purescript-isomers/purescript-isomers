module Isomers.Web.Spec where

import Isomers.Spec (class Builder, Spec, root) as Spec
import Isomers.Spec.Builder (SpecStep(..))
import Isomers.Web.Spec.Builder (class Builder, spec)
import Isomers.Web.Spec.Type (Spec(..))


root ∷ ∀ body a res rnd ireq oreq route.
  Builder a route (Spec.Spec body route ireq oreq res) rnd ⇒ Spec.Builder (Spec.Spec body route ireq oreq res) body route ireq oreq res ⇒ a → Spec (Spec.Spec body route ireq oreq res) rnd
root a = do
  let
    Spec { api, renderers } = spec (SpecStep ∷ SpecStep route) a

    api' = Spec.root api
  Spec { api: api', renderers }
