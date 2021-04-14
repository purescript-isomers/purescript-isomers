module Isomers.Web.Server where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
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
import Isomers.Web.Builder (Tagged(..))
import Isomers.Web.Renderer (Renderer(..))
import Isomers.Web.Types (WebSpec(..))
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert, set) as Record
import Type.Equality (class TypeEquals)
import Type.Equality (to) as Type.Equality
import Type.Prelude (class IsSymbol, SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype RenderHandlerStep router = RenderHandlerStep router

instance foldingRenderHandlerStep ∷
  ( Row.Cons ix { | subhandlers } handlers_ handlers
  , IsSymbol ix
  , HFoldlWithIndex (RenderHandlerStep router) { | subhandlers } { | rec } { | subhandlers' }
  , Row.Cons ix { | subhandlers' } handlers_ handlers'
  ) ⇒
  FoldingWithIndex (RenderHandlerStep router) (SProxy ix) { | handlers } { | rec } { | handlers' } where
  foldingWithIndex step ix handlers rec = do
    let
      subhandlers ∷ { | subhandlers }
      subhandlers = Record.get ix handlers
      subhandlers' = hfoldlWithIndex step subhandlers rec
    Record.set ix subhandlers' handlers

-- | We should parametrize this by "doc"
instance foldingRenderHandlerStepLeaf ∷
  ( Row.Cons ix { | mimeHandlers } handlers_ handlers
  , Row.Cons sourceMime (req → Aff res) mimeHandlers_ mimeHandlers
  , Row.Lacks HtmlMime mimeHandlers
  , Row.Cons HtmlMime (req → Aff (RawServer doc)) mimeHandlers mimeHandlers'
  -- | Why I'm not able to use mimeHandlers' type var here and below. Why??
  , Row.Cons ix { "text/html" ∷ req → Aff (RawServer doc) | mimeHandlers } handlers_ handlers'
  , TypeEquals f (Renderer router req res (RawServer doc))
  , IsSymbol ix
  , IsSymbol sourceMime
  ) ⇒
  FoldingWithIndex (RenderHandlerStep router) (SProxy ix) { | handlers } (Tagged sourceMime f) { | handlers' } where
  foldingWithIndex (RenderHandlerStep router) ix handlers (Tagged render) = do
    let
      mimeHandlers ∷ { | mimeHandlers }
      mimeHandlers = Record.get ix handlers

      sourceHandler ∷ req → Aff res
      sourceHandler = Record.get (SProxy ∷ SProxy sourceMime) mimeHandlers

      f' ∷ router /\ Exchange req res → RawServer doc
      f' = let Renderer f = Type.Equality.to render in f

      htmlHandler ∷ req → Aff (RawServer doc)
      htmlHandler req = f' <<< Tuple router <<< Exchange req <<< Just <<< Right <$> (sourceHandler req ∷ Aff res)

      -- | Here there is also a problem
      -- | mimeHandlers' ∷ { | mimeHandlers' }
      mimeHandlers' = Type.Equality.to (Record.insert _html htmlHandler mimeHandlers)

      handlers' ∷ { | handlers' }
      handlers' = Record.set ix mimeHandlers' handlers

    handlers'

renderToApi (WebSpec { render: HJust render, spec }) handlers router = do
  hfoldlWithIndex (RenderHandlerStep router) handlers render
