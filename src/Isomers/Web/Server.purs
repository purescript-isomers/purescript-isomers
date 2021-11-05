module Isomers.Web.Server where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.HTTP (Exchange(..))
import Isomers.HTTP.ContentTypes (HtmlMime, _html)
import Isomers.Response.Raw (RawServer)
import Isomers.Response.Types (HtmlString)
import Isomers.Web.Builder (Tagged(..))
import Isomers.Web.Renderer (Renderer(..))
import Isomers.Web.Types (WebSpec(..))
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert, set) as Record
import Type.Equality (class TypeEquals)
import Type.Equality (from, to) as Type.Equality
import Type.Prelude (class IsSymbol, Proxy(..))

-- | We are passing frontent router so components
-- | could trigger navigation etc.
data RenderHandlerStep m doc router
  = RenderHandlerStep (doc → m HtmlString) router

instance foldingRenderHandlerStep ∷
  ( Row.Cons ix { | subhandlers } handlers_ handlers
  , IsSymbol ix
  , HFoldlWithIndex (RenderHandlerStep m doc router) { | subhandlers } { | rec } { | subhandlers' }
  , Row.Cons ix { | subhandlers' } handlers_ handlers'
  ) ⇒
  FoldingWithIndex (RenderHandlerStep m doc router) (Proxy ix) { | handlers } { | rec } { | handlers' } where
  foldingWithIndex step ix handlers rec = do
    let
      subhandlers ∷ { | subhandlers }
      subhandlers = Record.get ix handlers

      subhandlers' = hfoldlWithIndex step subhandlers rec
    Record.set ix subhandlers' handlers

-- | We should parametrize this by "doc"
instance foldingRenderHandlerStepLeaf ∷
  ( Row.Cons ix { | mimeHandlers } handlers_ handlers
  , Row.Cons sourceMime (req → m res) mimeHandlers_ mimeHandlers
  , Row.Lacks HtmlMime mimeHandlers
  , Row.Cons HtmlMime (req → m (RawServer doc)) mimeHandlers mimeHandlers'
  -- | Why I'm not able to use mimeHandlers' type var here and below. Why??
  , Row.Cons ix { "text/html" ∷ req → m (RawServer HtmlString) | mimeHandlers } handlers_ handlers'

  , TypeEquals f (Renderer router req res (RawServer doc))
  -- , TypeEquals rrouter router
  -- , TypeEquals rreq req
  -- , TypeEquals rres res
  -- , TypeEquals rdoc doc
  , IsSymbol ix
  , IsSymbol sourceMime
  , Monad m
  ) ⇒
  FoldingWithIndex (RenderHandlerStep m doc router) (Proxy ix) { | handlers } (Tagged sourceMime f) { | handlers' } where
  foldingWithIndex (RenderHandlerStep renderToHtml router) ix handlers (Tagged f) = do
    let
      mimeHandlers ∷ { | mimeHandlers }
      mimeHandlers = Record.get ix handlers

      sourceHandler ∷ req → m res
      sourceHandler = Record.get (Proxy ∷ Proxy sourceMime) mimeHandlers

      -- f' ∷ (router /\ Exchange req res) → RawServer doc
      -- f' = dimap (bimap Type.Equality.from (bimap Type.Equality.from Type.Equality.from)) (map Type.Equality.to) f

      -- Renderer f' ∷ (router /\ Exchange req res) → RawServer doc
      Renderer f' = Type.Equality.to f

      f'' ∷ router /\ Exchange req res → m (RawServer HtmlString)
      f'' i = do
        let
          -- Renderer f = dimap (bimap Type.Equality.from (dimap Type.Equality.from Type.Equality.to)) (Type.Equality.to) $ render
          rs = f' i
        for rs renderToHtml
        -- renderToHtml <=< f

      htmlHandler ∷ req → m (RawServer HtmlString)
      htmlHandler req = f'' <<< Tuple router <<< Exchange req <<< Just <<< Right =<< (sourceHandler req ∷ m res)

      -- | Here there is also a problem
      -- | mimeHandlers' ∷ { | mimeHandlers' }
      mimeHandlers' = Type.Equality.to (Record.insert _html htmlHandler mimeHandlers)

      handlers' ∷ { | handlers' }
      handlers' = Record.set ix mimeHandlers' handlers
    handlers'

-- | TODO: As we don't use ireq / oreq / res here we have to probably remove WebSpec and use its pieces here.
-- |
-- | * `handlers` - record of functions into `m res`
-- | * `res` is included in the `router /\ Exchange req res` value and should be rendered into `RawServer doc` by spec renders
-- | * `doc` is taken by the provided final "page" wrapping render and turned into `HtmlString`
-- | * we end up with `RawServer HtmlString`
-- |
renderToApi ∷
  ∀ doc clientRouter body handlers handlers' m render ireq oreq res.
  HFoldlWithIndex (RenderHandlerStep m doc clientRouter) handlers render handlers' ⇒
  WebSpec body (HJust render) ireq oreq res →
  handlers →
  (doc → m HtmlString) →
  clientRouter →
  handlers'
renderToApi (WebSpec { render: HJust render, spec }) handlers renderToHtml router = do
  hfoldlWithIndex (RenderHandlerStep renderToHtml router) handlers render
