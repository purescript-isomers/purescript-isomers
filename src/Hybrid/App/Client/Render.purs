module Hybrid.App.Client.Render where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Hybrid.Api.Spec (FetchError(..), ResponseCodec(..))
import Hybrid.App.Renderer (Renderer)
import Hybrid.App.Spec (Raw(..)) as Spec
import Hybrid.HTTP.Exchange (Exchange(..), Result(..)) as HTTP.Exchange
import Hybrid.HTTP.Exchange (Exchange, Result(..), exchange) as HTTP
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (class IsSymbol, SProxy)

-- | We should put here `Spec` directly and not its piceses
data RenderFolding req res rnd
  = RenderFolding
    (Spec.Raw req res rnd)

instance renderFolding ::
  ( IsSymbol sym
  , Row.Cons sym (ResponseCodec res) response_ response
  , Row.Cons sym (Renderer req res doc) render_ render
  , Row.Cons sym req request_ request
  ) =>
  FoldingWithIndex
    (RenderFolding request response render)
    (SProxy sym)
    Unit
    (HTTP.Exchange req String)
    (Lazy doc) where
  foldingWithIndex (RenderFolding (Spec.Raw spec)) prop _ exch =
    let
      renderer = Record.get prop spec.renderers

      req /\ res = case exch of
        HTTP.Exchange.Ongoing req → req /\ Nothing
        HTTP.Exchange.Done req (HTTP.Exchange.Result res) → req /\ Just res

      ResponseCodec respCodec = Record.get prop spec.codecs.response

    in
        defer \_ →
          let
            res' = runExceptT do
              rawRes ← ExceptT res
              for rawRes \content → case respCodec.decode content of
                Just resp → pure resp
                Nothing → throwError (FetchError $ "Response decoding error: " <> content)
            result = HTTP.Result <$> res'
          in
            renderer $ HTTP.exchange req result

render ::
  ∀ doc rnd res req.
  HFoldlWithIndex (RenderFolding req res rnd) Unit (HTTP.Exchange (Variant req) String) doc ⇒
  Spec.Raw req res rnd →
  HTTP.Exchange (Variant req) String →
  doc
render spec@(Spec.Raw { codecs }) exch = do
  hfoldlWithIndex (RenderFolding spec) unit exch

