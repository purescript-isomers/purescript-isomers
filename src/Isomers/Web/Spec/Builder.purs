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
import Isomers.HTTP.Request.Method (Method(..))
import Isomers.Response (Duplex, RawDuplex') as Response
import Isomers.Response (Okayish)
import Isomers.Response.Raw.Duplexes (html) as Response.Raw.Duplexes
import Isomers.Response.Types (HtmlString)
import Isomers.Spec (class Builder, Spec, spec) as Spec
import Isomers.Spec.Builder (Insert(..), SpecStep(..))
import Isomers.Web.Spec.Type (GetApi, GetRenderers, Spec(..), _GetApi, _GetRenderers)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Type.Prelude (class IsSymbol, class TypeEquals, BProxy(..))
import Type.Row (RProxy(..))

class Builder a route api rnd | a route → api rnd where
  spec ∷ SpecStep route → a → Spec api rnd

data Rendered dpl rnd
  = Rendered dpl rnd

newtype Tagged (tag ∷ Symbol) a
  = Tagged a

data AppStep route
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

data FromHJust
  = FromHJust

instance mappingFromHJust ∷ Mapping FromHJust (HJust a) a where
  mapping _ (HJust a) = a

data DoBuildSpec route
  = DoBuildSpec (SpecStep route)

instance hevalDoBuildSpec ∷ (Spec.Builder a body route ireq oreq res) ⇒ HEval (DoBuildSpec route) (a → Spec.Spec body route ireq oreq res) where
  heval (DoBuildSpec step) i = Spec.spec step i

instance builderPlainEndpoint ∷
  ( HFoldl ExtractRenderStep HNothing res rnd
  , HEval DoIsHJust (rnd → BProxy rendered)
  , Spec.Builder (req /\ a) b route ireq oreq res'
  , HEval
      ( DoBuildSpec route
          H.<<< DoApply (a → req /\ a)
          H.<<< DoHIfThenElse (DoConst (BProxy rendered)) (DoApply (t → Response.RawDuplex' HtmlMime HtmlString : t)) DoIdentity
          H.<<< DoHMap DropRenderStep
      )
      (res → Spec.Spec b route ireq oreq res')
  ) ⇒
  Builder (req /\ res) route (Spec.Spec b route ireq oreq res') rnd where
  spec route (req /\ res) = do
    let
      rnd = hfoldl ExtractRenderStep HNothing res

      rendered = heval DoIsHJust rnd

      api =
        heval
          ( DoBuildSpec route
              H.<<< DoApply ((req /\ _) ∷ a → req /\ a)
              H.<<< DoHIfThenElse (DoConst rendered) (DoApply ((Response.Raw.Duplexes.html : _) ∷ t → _ : t)) DoIdentity
              H.<<< DoHMap DropRenderStep
          )
          res
    Spec { api, renderers: rnd }

instance builderSpec ∷
  Builder (Spec api rnd) route api rnd where
  spec _ s = s

type DoFoldRenderers a
  = ( DoHIfThenElse DoNull (DoConst HNothing) (DoApply (a → HJust a))
        H.<<< DoHMap FromHJust
        H.<<< DoHFilter CatMaybes
        H.<<< DoHMap GetRenderers
    )

_DoFoldRenderers ∷ ∀ a. DoFoldRenderers a
_DoFoldRenderers =
  ( DoHIfThenElse DoNull (DoConst HNothing) (DoApply (HJust ∷ a → HJust a))
      H.<<< DoHMap FromHJust
      H.<<< DoHFilter CatMaybes
      H.<<< DoHMap _GetRenderers
  )

instance builderMethod ∷
  ( HEval
      ( ( (DoBuildSpec route H.<<< DoApply (apis → Method apis) H.<<< DoHMap GetApi)
            H.&&& DoFoldRenderers a
        )
          H.<<< DoHMap (AppStep route)
      )
      ({ | rec } → Spec.Spec b route ireq oreq res /\ rnd)
  , Spec.Builder (Method apis) b route ireq oreq res
  ) ⇒
  Builder (Method { | rec }) route (Spec.Spec b route ireq oreq res) rnd where
  spec step (Method rec) = do
    let
      split =
        (DoBuildSpec step H.<<< DoApply (Method ∷ apis → Method apis) H.<<< DoHMap _GetApi)
          H.&&& (_DoFoldRenderers ∷ DoFoldRenderers a)

      api /\ rnd = heval (split H.<<< DoHMap (AppStep ∷ AppStep route)) rec
    Spec { api, renderers: rnd }

instance builderRec ∷
  ( HEval
        ((DoBuildSpec route H.<<< DoHMap GetApi H.&&& DoFoldRenderers a) H.<<< DoHMap (AppStep route))
        ({ | rec } → api /\ rnd)
    ) ⇒
  Builder { | rec } route api rnd where
  spec step rec = do
    let
      api /\ rnd =
        heval
          ((DoBuildSpec step H.<<< DoHMap _GetApi H.&&& (_DoFoldRenderers ∷ DoFoldRenderers a)) H.<<< DoHMap (AppStep ∷ AppStep route))
          rec
    Spec { api, renderers: rnd }

-- instance insertSpecBuilderSpec ∷
--   Builder (Insert l a (Spec.Spec b { | route } ireq oreq res)) { | route } (Spec.Spec b { | route } ireq oreq res) rnd where
--   spec s (Insert dpl sub) = do
--     let
--       Spec { api, renderers: rnd } = spec s sub
--       api' = Spec.spec s (Insert dpl api ∷ Insert l a (Spec.Spec b { | route } ireq oreq res))
--     Spec { api: api', renderers: rnd }
instance insertSpecBuilder ∷
  ( Builder sub { | route' } (Spec.Spec b { | route' } ireq oreq res) rnd
  , Row.Cons l a route route'
  , IsSymbol l
  , Row.Lacks l route
  , Spec.Builder (Insert l a (Spec.Spec b { | route' } ireq oreq res)) b { | route } ireq oreq res
  ) ⇒
  Builder (Insert l a sub) { | route } (Spec.Spec b { | route } ireq oreq res) rnd where
  spec s (Insert dpl sub) = do
    let
      s' = SpecStep ∷ SpecStep { | route' }
      Spec { api, renderers: rnd } = spec s' sub
      api' = Spec.spec s (Insert dpl api ∷ Insert l a (Spec.Spec b { | route' } ireq oreq res))
    Spec { api: api', renderers: rnd }

instance builderAppStep ∷ Builder a route api rnd ⇒ Mapping (AppStep route) a (Spec api rnd) where
  mapping _ a = spec (SpecStep ∷ SpecStep route) a

