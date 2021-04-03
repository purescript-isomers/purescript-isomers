module Isomers.Web.Spec.Builder where

import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class Mapping)
import Isomers.Contrib (HJust(..)) as Contrib
import Isomers.Contrib.Heterogeneous.Filtering (CatMaybes(..))
import Isomers.Contrib.Heterogeneous.HEval (class HEval, DoApply(..), DoConst(..), DoHFilter(..), DoHIfThenElse(..), DoHMap(..), DoIdentity(..), heval)
import Isomers.Contrib.Heterogeneous.HEval (type (<<<), (<<<), type (&&&), (&&&)) as H
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..), HNothing(..))
import Isomers.Contrib.Heterogeneous.List (type (:), (:))
import Isomers.HTTP.ContentTypes (HtmlMime)
import Isomers.Response (Duplex, RawDuplex') as Response
import Isomers.Response (Okayish)
import Isomers.Response.Raw.Duplexes (html) as Response.Raw.Duplexes
import Isomers.Response.Types (HtmlString)
import Isomers.Spec (class Builder, Spec, spec) as Spec
import Isomers.Web.Spec.Type (GetApi, GetRenderers, Spec(..), _GetApi, _GetRenderers)
import Prim.Boolean (False, True, kind Boolean)
import Prim.RowList (Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Type.Prelude (BProxy(..))

class Builder a api rnd | a → api rnd where
  spec ∷ a → Spec api rnd

data Rendered dpl rnd
  = Rendered dpl rnd

newtype Tagged (tag ∷ Symbol) a
  = Tagged a

data AppStep
  = AppStep

-- | From list of responses we extract a pair
-- | which defines response duplex and renderer.
data ExtractRenderStep
  = ExtractRenderStep

instance foldingExtractRenderMatch ∷
  Folding
    ExtractRenderStep
    acc
    (Rendered (Response.Duplex ct (Okayish vi i) (Okayish vo o)) rnd)
    (Contrib.HJust (Tagged ct rnd)) where
  folding _ _ (Rendered _ rnd) = Contrib.HJust (Tagged rnd)
else instance foldingExtractRenderNoMatch ∷
  Folding
    ExtractRenderStep
    acc
    resDpl
    acc where
  folding _ acc _ = acc

-- | Cleanup list of responses
data DropRenderStep
  = DropRenderStep

instance mappingDropRenderMatch ∷ Mapping DropRenderStep (Rendered resDpl rnd) resDpl where
  mapping _ (Rendered resDpl rnd) = resDpl
else instance mappingDropRenderNoMatch ∷ Mapping DropRenderStep resDpl resDpl where
  mapping _ resDpl = resDpl

class Null (rl ∷ RowList) (b ∷ Boolean) | rl → b

instance nullNil ∷ Null RL.Nil True
else instance nullCons ∷ Null l False

data DoNull
  = DoNull

instance hevalDoNull ∷ (RowToList r rl, Null rl b) ⇒ HEval DoNull ({ | r } → BProxy b) where
  heval _ _ = BProxy ∷ BProxy b

class IsHJust m (b ∷ Boolean) | m → b

instance isHJust ∷ IsHJust (HJust a) True
else instance isHJustNothing ∷ IsHJust HNothing False

data DoIsHJust
  = DoIsHJust

instance hevalDoIsHJust ∷ IsHJust i b ⇒ HEval DoIsHJust (i → BProxy b) where
  heval _ _ = BProxy ∷ BProxy b

data DoBuildSpec
  = DoBuildSpec

instance hevalDoBuildSpec ∷ (Spec.Builder a body acc req res) ⇒ HEval DoBuildSpec (a → Spec.Spec body acc req res) where
  heval _ i = Spec.spec i

instance builderPlainEndpoint ∷
  ( HFoldl ExtractRenderStep HNothing res rnd
  , HEval DoIsHJust (rnd → BProxy rendered)
  , Spec.Builder (req /\ a) b acc req' res'
  , HEval
      ( DoBuildSpec
          H.<<< DoApply (a → req /\ a)
          H.<<< DoHIfThenElse (DoConst (BProxy rendered)) (DoApply (t → Response.RawDuplex' HtmlMime HtmlString : t)) DoIdentity
          H.<<< DoHMap DropRenderStep
      )
      (res → Spec.Spec b acc req' res')
  ) ⇒
  Builder (req /\ res) (Spec.Spec b acc req' res') rnd where
  spec (req /\ res) = do
    let
      rnd = hfoldl ExtractRenderStep HNothing res

      rendered = heval DoIsHJust rnd

      api =
        heval
          ( DoBuildSpec
              H.<<< DoApply ((req /\ _) ∷ a → req /\ a)
              H.<<< DoHIfThenElse (DoConst rendered) (DoApply ((Response.Raw.Duplexes.html : _) ∷ t → _ : t)) DoIdentity
              H.<<< DoHMap DropRenderStep
          )
          res
    Spec { api, renderers: rnd }

instance builderRec ∷
  ( HEval
        ( ( DoBuildSpec H.<<< DoHMap GetApi
              H.&&& ( DoHIfThenElse DoNull (DoConst HNothing) (DoApply (a → HJust a))
                  H.<<< DoHFilter CatMaybes
                  H.<<< DoHMap GetRenderers
              )
          )
            H.<<< DoHMap AppStep
        )
        ({ | rec } → api /\ rnd)
    ) ⇒
  Builder { | rec } api rnd where
  spec rec = do
    let
      api /\ rnd =
        heval
          ( ( DoBuildSpec H.<<< DoHMap _GetApi
                H.&&& ( DoHIfThenElse DoNull (DoConst HNothing) (DoApply (HJust ∷ a → HJust a))
                      H.<<< DoHFilter CatMaybes
                      H.<<< DoHMap _GetRenderers
                  )
            )
              H.<<< DoHMap AppStep
          )
          rec
    Spec { api, renderers: rnd }

instance builderAppStep ∷ Builder a api rnd ⇒ Mapping AppStep a (Spec api rnd) where
  mapping _ a = spec a

