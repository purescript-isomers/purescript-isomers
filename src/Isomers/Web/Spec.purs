module Isomers.Web.Spec where

import Isomers.Spec (class Builder, Spec(..), root) as Spec
import Isomers.Web.Spec.Builder (class Builder, spec)
import Isomers.Web.Spec.Type (Spec(..))


root ∷ ∀ body acc api a i rb req res rnd.
  Builder a (Spec.Spec body acc req res) rnd ⇒ Spec.Builder (Spec.Spec body acc req res) body acc req res ⇒ a → Spec (Spec.Spec body acc req res) rnd
root a = do
  let
    Spec { api, renderers } = spec a

    api' = Spec.root api
  Spec { api: api', renderers }
