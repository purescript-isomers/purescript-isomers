module Isomers.Web.Client.Render where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Symbol (reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, default, expand, inj, match, on) as Variant
import Data.Variant (class VariantMatchCases, Variant)
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Isomers.Client (ClientStep, RequestBuildersStep) as Client
import Isomers.Client.Fetch (HostInfo)
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.HTTP.ContentTypes (HtmlMime, _html)
import Isomers.HTTP.Exchange (Error) as HTTP.Exchange
import Isomers.HTTP.Exchange (Exchange(..)) as HTTP
import Isomers.Spec (Spec(..))
import Isomers.Spec (client) as Spec
import Isomers.Web.Builder (Tagged(..))
import Isomers.Web.Renderer (Renderer(..))
import Isomers.Web.Types (WebSpec(..))
import Prelude (bind, const, map, pure, (#), ($), (<<<))
import Prim.Row (class Cons, class Union) as Row
import Prim.RowList (class RowToList)
import Record (get) as Record
import Type.Equality (from, to) as Type.Equality
import Type.Prelude (class IsSymbol, class TypeEquals, Proxy, SProxy(..))

-- | * We map the renderers `Record` (which is in some sens a subtree of
-- | the spec request `Variant` tree) recursively.
-- |
-- | * Bottom up from every from this `hmap`ping we build a set of handlers
-- | for the render request type which we define along side this fold.
-- |
-- | * The render request `Variant` type is really close to the render `Record`.
-- | It contains a value of the `request` from the original request `Variant`
-- | in place of a `Tagged` rendering function.
-- |
-- | * So our fold picks a client function bsad on `Tagged` content type symbol
-- | to perform the request when a value from the render `Variant` request type
-- | is recursively matched over this new folded record.
-- |
-- | * We take the response data and feed them into the rendering function
-- | which our `Tagged` value contains.
-- |
-- | * This produces a `Response.RawServer doc` which we can strip down
-- | to just `doc` and use in our router.
-- |
-- | * This is not entirely true because this mapping doesn't fold the top
-- | layer. The final match is done in the `foldRender` function. I've put
-- | this in a `class` to accumulate the intermediate constraints and
-- | simplify signatures.
data RenderStep clientRouter client = RenderStep client

instance mappingRenderLeaf ::
  ( IsSymbol sym
  , IsSymbol ct
  , Row.Cons sym { | mimeClient } client_ client
  , Row.Cons ct (creq → Aff (Either HTTP.Exchange.Error cres)) mimeClient_ mimeClient
  , TypeEquals creq rreq
  , TypeEquals cres rres
  , TypeEquals rcr clientRouter
  ) =>
  MappingWithIndex
    (RenderStep clientRouter { | client })
    (SProxy sym)
    (Tagged ct (Renderer rcr rreq rres doc))
    (creq → clientRouter → Aff doc) where
  mappingWithIndex (RenderStep client) prop (Tagged (Renderer renderer)) creq clientRouter = do
    let
      ct = SProxy ∷ SProxy ct
      fetch = Record.get ct (Record.get prop client)
    res ← fetch creq
    pure $ renderer (Type.Equality.from clientRouter /\ HTTP.Exchange (Type.Equality.to $ creq) (Just $ map Type.Equality.to res))

instance mappingRenderNode ::
  ( IsSymbol sym
  , Row.Cons sym subclient client_ client
  , HMapWithIndex (RenderStep clientRouter subclient) { | renderers } ({ | renderers' })
  , RowToList renderers' rl
  , VariantMatchCases rl vreq_ (clientRouter → Aff doc)
  , Row.Union vreq_ () vreq
  ) =>
  MappingWithIndex
    (RenderStep clientRouter { | client })
    (SProxy sym)
    { | renderers }
    (Variant vreq → clientRouter → Aff doc) where
  mappingWithIndex (RenderStep client) prop renderers = do
    let
      subclient = Record.get prop client
      renderers' ∷ { | renderers' }
      renderers' = hmapWithIndex (RenderStep subclient ∷ RenderStep clientRouter subclient) renderers

    Variant.match renderers'

-- | Handy alias
class FoldRender spec clientRouter rndReq doc | spec → rndReq doc where
  foldRender ∷ Proxy clientRouter → spec → HostInfo → (rndReq → clientRouter → doc)

instance instanceFoldRender ∷
  ( HFoldlWithIndex (ExpandRequest ireq) (Variant () → Variant ireq) rnd (Variant rndReq → Variant ireq)
  , HFoldlWithIndex (Client.RequestBuildersStep (Variant ireq) (Variant ireq)) {} (Proxy (Variant ireq)) requestBuilders
  , HFoldlWithIndex (Client.ClientStep (Variant ireq) res) {} requestBuilders { | client }
  , HMapWithIndex (RenderStep clientRouter { | client }) rnd ({ | rnd' })
  , RowToList rnd' rndrl'
  , VariantMatchCases rndrl' rndReq (clientRouter → Aff doc)
  , Row.Union rndReq () rndReq
  ) ⇒ FoldRender (WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res) clientRouter (Variant rndReq) (Aff doc) where
    foldRender _ webSpec@(WebSpec { spec: spec@(Spec { request: reqDpl, response }), render: HJust render }) hostInfo = do
      let
        client ∷ { | client }
        client = Spec.client hostInfo spec

        fetchAndRender = hmapWithIndex (RenderStep client ∷ RenderStep clientRouter { | client }) (render ∷ rnd)
      Variant.match fetchAndRender


-- | We contracting here route `Variant` but also
-- | a render request `Variant` into data source
-- | request.
data ContractRequest (specReq ∷ # Type) = ContractRequest

instance foldingContractRequest ∷
  ( IsSymbol sym
  , IsSymbol ct
  , Row.Cons sym (Variant mimeVReq) vReq_ vReq
  , Row.Cons ct req ("text/html" ∷ req | mimeVReq_) mimeVReq
  , Row.Cons sym req rndReq rndReq'
  , Row.Union rndReq rndReq_ rndReq'
  , Row.Union vReq_ vReq__ vReq
  ) =>
  FoldingWithIndex (ContractRequest vReq) (SProxy sym) (Variant vReq → Maybe (Variant rndReq)) (Tagged ct rnd) (Variant vReq → Maybe (Variant rndReq')) where
  foldingWithIndex _ prop contractAcc _ = do
    let
      ct = SProxy ∷ SProxy ct
      expandRndReq = Variant.expand ∷ Variant rndReq → Variant rndReq'
      expandVReq = Variant.expand ∷ Variant vReq_ → Variant vReq

    Variant.on prop
      ((Variant.default Nothing)
        # Variant.on _html (Just <<< Variant.inj prop)
        # Variant.on ct (Just <<< Variant.inj prop)
      )
      (map expandRndReq <<< contractAcc <<< expandVReq)

instance foldingContractRequestRec ∷
  ( IsSymbol sym
  , Row.Cons sym (Variant specSubReq) specReq_ specReq
  , HFoldlWithIndex (ContractRequest specSubReq) (Variant specSubReq → Maybe (Variant ())) { | renderers } (Variant specSubReq → Maybe renderers')
  , Row.Cons sym renderers' rndReq rndReq'
  , Row.Union rndReq rndReq_ rndReq'
  , Row.Union specReq_ specReq__ specReq
  ) ⇒
  FoldingWithIndex (ContractRequest specReq) (SProxy sym) (Variant specReq → Maybe (Variant rndReq)) { | renderers } (Variant specReq → Maybe (Variant rndReq')) where
  foldingWithIndex _ prop contractSpecReq v = do
    let
      cr = ContractRequest ∷ ContractRequest specSubReq

      expandRndReq = Variant.expand ∷ Variant rndReq → Variant rndReq'
      expandSpecReq = Variant.expand ∷ Variant specReq_ → Variant specReq

      contractSubrenders ∷ Variant specSubReq → Maybe (Variant rndReq')
      contractSubrenders =
        map (Variant.inj prop) <<< hfoldlWithIndex cr ((const $ Nothing) ∷ Variant specSubReq → Maybe (Variant ())) v

      contractRest ∷ Variant specReq_ → Maybe (Variant rndReq')
      contractRest = map expandRndReq <<< contractSpecReq <<< expandSpecReq

    Variant.on prop
      contractSubrenders
      contractRest

-- | Contract "server side" (but we perform this also on the client side
-- | when route parsing is required) decoded request into a "render request".
contractRequest ∷ ∀ oreq renderers rndReq.
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) renderers (Variant oreq → Maybe (Variant rndReq)) ⇒
  Proxy (Variant oreq) →
  renderers →
  (Variant oreq → Maybe (Variant rndReq))
contractRequest _ renderers = hfoldlWithIndex (ContractRequest ∷ ContractRequest oreq) (const Nothing ∷ Variant oreq → Maybe (Variant ())) renderers

data ExpandRequest (specReq ∷ # Type) = ExpandRequest

instance foldingExpandRequest ∷
  ( IsSymbol sym
  , IsSymbol ct
  , Row.Cons ct req specSubReq_ specSubReq
  , Row.Cons sym (Variant specSubReq) specReq_ specReq
  , Row.Cons sym req rndReq rndReq'
  ) ⇒
  FoldingWithIndex (ExpandRequest specReq) (SProxy sym) (Variant rndReq → Variant specReq) (Tagged ct rnd) (Variant rndReq' → Variant specReq) where
  foldingWithIndex _ prop expandRndReq tagged = do
    let
      ct = SProxy ∷ SProxy ct
    Variant.on prop (Variant.inj prop <<< Variant.inj ct) expandRndReq

instance foldingExpandRequestRec ∷
  ( IsSymbol sym
  , HFoldlWithIndex (ExpandRequest specSubReq) (Variant () → Variant specSubReq) { | renderers } (Variant rndSubReq → Variant specSubReq)
  , Row.Cons sym (Variant specSubReq) specReq_ specReq
  , Row.Cons sym (Variant rndSubReq) rndReq rndReq'
  ) ⇒
  FoldingWithIndex (ExpandRequest specReq) (SProxy sym) (Variant rndReq → Variant specReq) { | renderers } (Variant rndReq' → Variant specReq) where
  foldingWithIndex _ prop expandRndReq rs = do
    let
      expandRndSubReq ∷ Variant rndSubReq → Variant specSubReq
      expandRndSubReq = hfoldlWithIndex (ExpandRequest ∷ ExpandRequest specSubReq) (Variant.case_ ∷ Variant () → Variant specSubReq) rs
    Variant.on
      prop
      (Variant.inj prop <<< expandRndSubReq)
      expandRndReq

-- | Expand "client side" encoded request into a "render request".
expandRequest ∷
  ∀ ireq renderers rndReq.
  HFoldlWithIndex (ExpandRequest ireq) (Variant () → Variant ireq) renderers (Variant rndReq → Variant ireq) =>
  Proxy (Variant ireq) →
  renderers →
  (Variant rndReq → Variant ireq)
expandRequest _ = hfoldlWithIndex (ExpandRequest ∷ ExpandRequest ireq) (Variant.case_ ∷ Variant () → Variant ireq)
