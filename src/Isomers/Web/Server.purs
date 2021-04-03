module Isomers.Web.Server where

import Prelude

import Effect.Aff (Aff)
import Heterogeneous.Folding (class Folding) as Heterogeneous
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class MappingWithIndex, hmap)
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.HTTP.ContentTypes (HtmlMime, _html)
import Isomers.Response.Raw (RawServer(..))
import Isomers.Response.Types (HtmlString(..))
import Isomers.Server (router) as Server
import Isomers.Web.Spec.Builder (Tagged(..))
import Isomers.Web.Spec.Type (Spec(..))
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert, set) as Record
import Test.Spec (handlers)
import Type.Equality (class TypeEquals)
import Type.Equality (to) as Type.Equality
import Type.Prelude (class IsSymbol, SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

data RenderHandlerStep = RenderHandlerStep

instance foldingRenderHandlerStep ∷
  ( Row.Cons ix { | subhandlers } handlers_ handlers
  , IsSymbol ix
  , HFoldlWithIndex RenderHandlerStep { | subhandlers } { | rec } { | subhandlers' }
  , Row.Cons ix { | subhandlers' } handlers_ handlers'
  ) ⇒
  FoldingWithIndex RenderHandlerStep (SProxy ix) { | handlers } { | rec } { | handlers' } where
  foldingWithIndex RenderHandlerStep ix handlers rec = do
    let
      subhandlers ∷ { | subhandlers }
      subhandlers = Record.get ix handlers
      subhandlers' = hfoldlWithIndex RenderHandlerStep subhandlers rec
    Record.set ix subhandlers' handlers

-- | We should parametrize this by "doc"
instance foldingRenderHandlerStepLeaf ∷
  ( Row.Cons ix { | mimeHandlers } handlers_ handlers
  , Row.Cons sourceMime (a → m r) mimeHandlers_ mimeHandlers
  , Row.Lacks HtmlMime mimeHandlers
  , Row.Cons HtmlMime (a → m r') mimeHandlers mimeHandlers'
  -- | Why I'm not able to use mimeHandlers' here??
  , Row.Cons ix { "text/html" ∷ a → m (RawServer HtmlString) | mimeHandlers } handlers_ handlers'
  , TypeEquals f (r → RawServer HtmlString)
  , Functor m
  , IsSymbol ix
  , IsSymbol sourceMime
  ) ⇒
  FoldingWithIndex RenderHandlerStep (SProxy ix) { | handlers } (Tagged sourceMime f) { | handlers' } where
  foldingWithIndex RenderHandlerStep ix handlers (Tagged render) = do
    let
      mimeHandlers ∷ { | mimeHandlers }
      mimeHandlers = Record.get ix handlers

      sourceHandler ∷ a → m r
      sourceHandler = Record.get (SProxy ∷ SProxy sourceMime) mimeHandlers

      f' = Type.Equality.to render

      htmlHandler ∷ a → m (RawServer HtmlString)
      htmlHandler a = f' <$> (sourceHandler a ∷ m r)

      mimeHandlers' ∷ { "text/html" ∷ a → m (RawServer HtmlString) | mimeHandlers }
      mimeHandlers' = Record.insert _html htmlHandler mimeHandlers

      handlers' ∷ { | handlers' }
      handlers' = Record.set ix mimeHandlers' handlers

    handlers'

renderToApi spec@(Spec { api, renderers: HJust renderers }) handlers = do
  hfoldlWithIndex RenderHandlerStep handlers renderers
