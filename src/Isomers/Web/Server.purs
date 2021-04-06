module Isomers.Web.Server where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Heterogeneous.Folding (class Folding) as Heterogeneous
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class MappingWithIndex, hmap)
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.HTTP (Exchange(..))
import Isomers.HTTP.ContentTypes (HtmlMime, _html)
import Isomers.Response.Raw (RawServer(..))
import Isomers.Response.Types (HtmlString(..))
import Isomers.Server (router) as Server
import Isomers.Web.Spec.Builder (Tagged(..))
import Isomers.Web.Spec.Type (Spec(..))
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert, set) as Record
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
  , Row.Cons sourceMime (req → m res) mimeHandlers_ mimeHandlers
  , Row.Lacks HtmlMime mimeHandlers
  , Row.Cons HtmlMime (req → m (RawServer doc)) mimeHandlers mimeHandlers'
  -- | Why I'm not able to use mimeHandlers' here and below??
  , Row.Cons ix { "text/html" ∷ req → m (RawServer doc) | mimeHandlers } handlers_ handlers'
  , TypeEquals f (Exchange req res → RawServer doc)
  , Functor m
  , IsSymbol ix
  , IsSymbol sourceMime
  ) ⇒
  FoldingWithIndex RenderHandlerStep (SProxy ix) { | handlers } (Tagged sourceMime f) { | handlers' } where
  foldingWithIndex RenderHandlerStep ix handlers (Tagged render) = do
    let
      mimeHandlers ∷ { | mimeHandlers }
      mimeHandlers = Record.get ix handlers

      sourceHandler ∷ req → m res
      sourceHandler = Record.get (SProxy ∷ SProxy sourceMime) mimeHandlers

      f' ∷ Exchange req res → RawServer doc
      f' = Type.Equality.to render

      htmlHandler ∷ req → m (RawServer doc)
      htmlHandler req = f' <<< Exchange req <<< Just <<< Right <$> (sourceHandler req ∷ m res)

      -- | Here there is also a problem
      -- | mimeHandlers' ∷ { | mimeHandlers' }
      mimeHandlers' = Type.Equality.to (Record.insert _html htmlHandler mimeHandlers)

      handlers' ∷ { | handlers' }
      handlers' = Record.set ix mimeHandlers' handlers

    handlers'

renderToApi spec@(Spec { api, renderers: HJust renderers }) handlers = do
  hfoldlWithIndex RenderHandlerStep handlers renderers
