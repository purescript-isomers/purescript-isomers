module Hybrid.App.Client where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Hybrid.Api.Spec (FetchError(..), ResponseCodec(..))
import Hybrid.App.Renderer (Renderer)
import Hybrid.App.Spec (Raw(..)) as Spec
import Hybrid.Response (Response)
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Request.Duplex (Request, print) as Request.Duplex
import Type.Prelude (class IsSymbol, SProxy)

type HTTPFetch m = Request.Duplex.Request → m (FetchError \/ Response String)

-- | We should put here `Spec` directly and not its piceses
data Route m req res rnd
  = Route
    (HTTPFetch m)
    (Spec.Raw req res rnd)

newtype Renders m doc
  = Renders
  { pre ∷ Lazy doc
  , fetch ∷ m doc
  }

instance routeFold ::
  ( IsSymbol sym
  , Row.Cons sym (ResponseCodec res) response_ response
  , Row.Cons sym (Renderer req res doc) render_ render
  , Row.Cons sym req request_ request
  , Monad m
  ) =>
  FoldingWithIndex
    (Route m request response render)
    (SProxy sym)
    Unit
    req
    (Renders m doc) where
  foldingWithIndex (Route fetch (Spec.Raw spec)) prop _ req =
    let
      renderer = Record.get prop spec.renderers

      ResponseCodec respCodec = Record.get prop spec.codecs.response
    in
      Renders
        { pre: defer \_ → renderer req Nothing
        , fetch:
            do
              let
                rawReq = Request.Duplex.print spec.codecs.request (Variant.inj prop req)
              resp ← fetch rawReq <#> case _ of
                Right resp → for resp \content → case respCodec.decode content of
                  Just resp' → pure resp'
                  Nothing → Left (FetchError $ "Response decoding error: " <> content)
                Left err → Left err
              pure (renderer req (Just resp))
        }

route ::
  ∀ doc rnd res req m.
  Monad m ⇒
  HFoldlWithIndex (Route m req res rnd) Unit (Variant req) (Renders m doc) ⇒
  HTTPFetch m →
  Spec.Raw req res rnd →
  Variant req →
  Renders m doc
route rawFetch spec = hfoldlWithIndex (Route rawFetch spec) unit
