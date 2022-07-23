module Isomers.Client where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Effect.Aff (Aff)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
-- import Isomers.Client.Fetch (HostInfo, fetch)
import Isomers.HTTP.Exchange (Error(..)) as HTTP.Exchange
import Isomers.Request (Duplex(..), Printer) as Request
import Isomers.Request.Duplex.Printer (run) as Request.Duplex.Printer
import Isomers.Request.Encodings (ClientRequest) as Request.Encodings
import Isomers.Response (Duplex, parse) as Response
import Isomers.Response.Encodings (ClientResponse) as Response.Encodings
import JS.Unsafe.Stringify (unsafeStringify)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList)
import Record (get, insert) as Record
import Type.Prelude (class IsSymbol, Proxy(..), Proxy(..), Proxy)

-- | This folding creates a request builder:
-- | a record which contains functions which put a
-- | request value (nested Variant) togheter based
-- | on the labels from the path.
-- |
-- | TODO: Extract this into a separate generic lib / tool.
data RequestBuildersStep a ireq = RequestBuildersStep (a -> ireq)

instance requestFoldingVariant ::
  ( IsSymbol sym
  , RowToList v vl
  , Row.Cons sym (Variant v) curr_ curr
  , Row.Lacks sym requestBuilders
  , Row.Cons sym { | subrequestBuilders } requestBuilders requestBuilders'
  , HFoldlWithIndex (RequestBuildersStep (Variant v) request) {} (Proxy vl) { | subrequestBuilders }
  ) =>
  FoldingWithIndex
    (RequestBuildersStep (Variant curr) request)
    (Proxy sym)
    { | requestBuilders }
    (Proxy (Variant v))
    { | requestBuilders' } where
  foldingWithIndex (RequestBuildersStep inj) prop rb _ = do
    let
      f :: Variant v -> Variant curr
      f = Variant.inj prop

      inj' = inj <<< f

      subrequestBuilders = hfoldlWithIndex (RequestBuildersStep inj') {} (Proxy :: Proxy vl)
    Record.insert prop subrequestBuilders rb
else instance requestFoldingData ::
  ( IsSymbol sym
  , Row.Lacks sym requestBuilders
  , Row.Cons sym (d -> request) requestBuilders requestBuilders'
  , Row.Cons sym d r_ r
  ) =>
  FoldingWithIndex
    (RequestBuildersStep (Variant r) request)
    (Proxy sym)
    { | requestBuilders }
    (Proxy d)
    { | requestBuilders' } where
  foldingWithIndex (RequestBuildersStep inj) prop rb d = do
    let
      inj' = inj <<< Variant.inj prop
    Record.insert prop inj' rb

-- | Direct `HFoldlWithIndex` instances which trigger
-- | the actual foldings for the above `RequestBuildersStep`
-- | by passing appropriate proxies
-- | into the fold.
instance hfoldlWithIndexRequestBuildersStepVariantWrapper ::
  ( HFoldlWithIndex (RequestBuildersStep (Variant v) request) {} (Proxy vl) { | requestBuilders }
  , Newtype (f (Variant v)) (Variant v)
  , RowToList v vl
  ) =>
  HFoldlWithIndex (RequestBuildersStep (f (Variant v)) request) unit (Proxy (f (Variant v))) { | requestBuilders } where
  hfoldlWithIndex (RequestBuildersStep f) init _ = hfoldlWithIndex (RequestBuildersStep (f <<< wrap)) {}
    (Proxy :: Proxy vl)
else instance hfoldlWithIndexRequestBuildersStepVariant ::
  ( HFoldlWithIndex (RequestBuildersStep (Variant v) request) {} (Proxy vl) { | requestBuilders }
  , RowToList v vl
  ) =>
  HFoldlWithIndex (RequestBuildersStep (Variant v) request) unit (Proxy (Variant v)) { | requestBuilders } where
  hfoldlWithIndex cf init _ = hfoldlWithIndex cf {} (Proxy :: Proxy vl)

type Fetch = Request.Encodings.ClientRequest -> Aff (Either String Response.Encodings.ClientResponse)

-- | This folding creates the actual client which is able to perform
-- | network requests etc.
data ClientStep request responseDuplexes = ClientStep Fetch (request -> Request.Printer) responseDuplexes

-- | TODO: parameterize the client by fetching function
-- | so the whole exchange can be performed "purely".
instance clientFoldingResponseConstructor ::
  ( IsSymbol sym
  , Row.Lacks sym client
  , Row.Cons sym (Response.Duplex ct i res) resDpls_ resDpls
  , Row.Cons sym (d -> Aff (Either HTTP.Exchange.Error res)) client client'
  ) =>
  FoldingWithIndex
    (ClientStep request { | resDpls })
    (Proxy sym)
    { | client }
    (d -> request)
    { | client' } where
  foldingWithIndex (ClientStep fetch reqPrt resDpls) prop c reqBld = do
    let
      resDpl = Record.get prop resDpls
      f d = do
        let r = reqBld d
        res <- fetch (Request.Duplex.Printer.run (reqPrt r)) >>= case _ of
          Right res -> lmap (HTTP.Exchange.Error <<< unsafeStringify) <$> Response.parse resDpl res
          Left err -> pure $ Left $ HTTP.Exchange.Error err
        pure $ res
    Record.insert prop f c
else instance clientFoldingResponseDuplexRec ::
  ( IsSymbol sym
  , Row.Lacks sym client
  , Row.Cons sym subclient client client'
  , Row.Cons sym subResponseDuplexes responseDuplexes_ responseDuplexes
  , HFoldlWithIndex (ClientStep request subResponseDuplexes) {} { | requestBuilders } subclient
  ) =>
  FoldingWithIndex
    (ClientStep request { | responseDuplexes })
    (Proxy sym)
    { | client }
    { | requestBuilders }
    { | client' } where
  foldingWithIndex (ClientStep fetch reqPrt resDpls) prop c reqBld = do
    let
      sResDpls = Record.get prop resDpls

      subclient = hfoldlWithIndex (ClientStep fetch reqPrt sResDpls) {} reqBld

    Record.insert prop subclient c
else instance clientFoldingResponseDuplexNewtypeWrapper ::
  ( Newtype (f responseDuplexes) responseDuplexes
  , FoldingWithIndex (ClientStep request responseDuplexes) (Proxy sym) { | client } { | requestBuilders } { | client' }
  ) =>
  FoldingWithIndex
    (ClientStep request (f responseDuplexes))
    (Proxy sym)
    { | client }
    { | requestBuilders }
    { | client' } where
  foldingWithIndex (ClientStep fetch reqPrt resDpl) prop c reqBld = do
    foldingWithIndex (ClientStep fetch reqPrt (unwrap resDpl)) prop c reqBld

-- | Pass a proxy for your nested request `Variant` type to get back
-- | a nested `Record` with functions which builds a given `Variant`.
-- |
-- | This is a bit low level and you probably want to use helpers for
-- | specs which provide this type and proxy for you.
requestBuilders
  :: forall requestBuilders t
   . HFoldlWithIndex (RequestBuildersStep t t) {} (Proxy t) { | requestBuilders }
  => Proxy t
  -> { | requestBuilders }
requestBuilders = hfoldlWithIndex (RequestBuildersStep (identity :: t -> t)) {}

-- | TODO: In this folding we pass response duplex around but we need only response parser.
-- | Perform a recursive heterogeneous map which extracts only parsers and use this cleaner
-- | value for `ClientStep` folding.
client
  :: forall body client requestBuilders responseDuplexes ireq oreq
   . HFoldlWithIndex (RequestBuildersStep ireq ireq) {} (Proxy ireq) { | requestBuilders }
  => HFoldlWithIndex (ClientStep ireq responseDuplexes) {} { | requestBuilders } client
  => Fetch
  -> Request.Duplex body ireq oreq
  -> responseDuplexes
  -> client
client fetch (Request.Duplex reqPrt _) resDpl = do
  let
    rb :: { | requestBuilders }
    rb = requestBuilders (Proxy :: Proxy ireq)
  hfoldlWithIndex (ClientStep fetch reqPrt resDpl) {} rb :: client

