module Isomers.Client where

import Prelude

import Control.Bind.Indexed (ibind)
import Control.Comonad (extract) as Comonad
import Control.Monad.Except (throwError)
import Control.Monad.Free.Trans (liftFreeT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Identity (Identity(..))
import Data.Lazy (defer) as Lazy
import Data.Lens (view)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant (case_, inj) as Variant
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (new, read, write) as Ref
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, hmapWithIndex, mappingWithIndex)
import Isomers.Client.Fetch (HostInfo, Scheme(..), fetch)
import Isomers.HTTP (Exchange(..)) as HTTP
import Isomers.HTTP.Exchange (Error(..)) as HTTP.Exchange
import Isomers.Request (Duplex(..), Duplex', Printer(..)) as Request
import Isomers.Request.Duplex (parse, print) as Request.Duplex
import Isomers.Request.Duplex.Printer (run) as Request.Duplex.Printer
import Isomers.Request.Encodings (ServerRequest)
import Isomers.Request.Encodings (ServerRequest) as Request.Encodings
import Isomers.Response (Duplex(..), parse) as Response
import Isomers.Response.Encodings (ClientResponse(..)) as Response.Encodings
import Isomers.Response.Raw (RawClient(..), RawServer(..)) as Response
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList)
import React.Basic (JSX)
import React.Basic.Hooks (component)
import Record (get, insert) as Record
import Routing.PushState (makeInterface)
import Routing.PushState (makeInterface) as PushState
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Wire.React.Router (Transition, _Transition, continue, makeRouter)
import Wire.React.Router.Control (Command(..), Resolved, Router(..), Transition(..), Transitioning) as Router
import Wire.Signal (Signal)
import Wire.Signal (create) as Signal

-- | This folding creates a request builder:
-- | a record which contains functions which put a
-- | request value (nested Variant) togheter based
-- | on the labels from the path.
-- |
-- | TODO: Extract this into a separate generic lib / tool.
data RequestBuildersStep a ireq
  = RequestBuildersStep (a → ireq)

instance requestFoldingVariant ::
  ( IsSymbol sym
  , RowToList v vl
  , Row.Cons sym (Variant v) curr_ curr
  , Row.Lacks sym requestBuilders
  , Row.Cons sym { | subrequestBuilders } requestBuilders requestBuilders'
  , HFoldlWithIndex (RequestBuildersStep (Variant v) request) {} (RLProxy vl) { | subrequestBuilders }
  ) =>
  FoldingWithIndex
    (RequestBuildersStep (Variant curr) request)
    (SProxy sym)
    { | requestBuilders }
    (Proxy (Variant v))
    { | requestBuilders' } where
  foldingWithIndex (RequestBuildersStep inj) prop rb _ = do
    let
      f ∷ Variant v → Variant curr
      f = Variant.inj prop

      inj' = inj <<< f

      subrequestBuilders = hfoldlWithIndex (RequestBuildersStep inj') {} (RLProxy ∷ RLProxy vl)
    Record.insert prop subrequestBuilders rb
else instance requestFoldingData ::
  ( IsSymbol sym
  , Row.Lacks sym requestBuilders
  , Row.Cons sym (d → request) requestBuilders requestBuilders'
  , Row.Cons sym d r_ r
  ) =>
  FoldingWithIndex
    (RequestBuildersStep (Variant r) request)
    (SProxy sym)
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
instance hfoldlWithIndexRequestBuildersStepVariantWrapper ∷
  ( HFoldlWithIndex (RequestBuildersStep (Variant v) request) {} (RLProxy vl) { | requestBuilders }
  , Newtype (f (Variant v)) (Variant v)
  , RowToList v vl
  ) ⇒
  HFoldlWithIndex (RequestBuildersStep (f (Variant v)) request) unit (Proxy (f (Variant v))) { | requestBuilders } where
  hfoldlWithIndex (RequestBuildersStep f) init _ = hfoldlWithIndex (RequestBuildersStep (f <<< wrap)) {} (RLProxy ∷ RLProxy vl)
else instance hfoldlWithIndexRequestBuildersStepVariant ∷
  ( HFoldlWithIndex (RequestBuildersStep (Variant v) request) {} (RLProxy vl) { | requestBuilders }
  , RowToList v vl
  ) ⇒
  HFoldlWithIndex (RequestBuildersStep (Variant v) request) unit (Proxy (Variant v)) { | requestBuilders } where
  hfoldlWithIndex cf init _ = hfoldlWithIndex cf {} (RLProxy ∷ RLProxy vl)

-- | This folding creates the actual client which is able to perform
-- | network requests etc.
data ClientStep request responseDuplexes
  = ClientStep HostInfo (request → Request.Printer) responseDuplexes

-- | TODO: parameterize the client by fetching function
-- | so the whole exchange can be performed "purely".
instance clientFoldingResponseConstructor ∷
  ( IsSymbol sym
  , Row.Lacks sym client
  , Row.Cons sym (Response.Duplex ct i res) resDpls_ resDpls
  , Row.Cons sym (d → Aff (Either HTTP.Exchange.Error res)) client client'
  ) =>
  FoldingWithIndex
    (ClientStep request { | resDpls })
    (SProxy sym)
    { | client }
    (d → request)
    { | client' } where
  foldingWithIndex (ClientStep hostInfo reqPrt resDpls) prop c reqBld = do
    let
      resDpl = Record.get prop resDpls
      f d = do
        let r = reqBld d
        res ← fetch hostInfo (Request.Duplex.Printer.run (reqPrt r)) >>= case _ of
          Right res → lmap (HTTP.Exchange.Error <<< unsafeStringify) <$> Response.parse resDpl res
          Left err → pure $ Left $ HTTP.Exchange.Error err
        pure $ res
    Record.insert prop f c
else instance clientFoldingResponseDuplexRec ∷
  ( IsSymbol sym
  , Row.Lacks sym client
  , Row.Cons sym subclient client client'
  , Row.Cons sym subResponseDuplexes responseDuplexes_ responseDuplexes
  , HFoldlWithIndex (ClientStep request subResponseDuplexes) {} { | requestBuilders } subclient
  ) =>
  FoldingWithIndex
    (ClientStep request { | responseDuplexes })
    (SProxy sym)
    { | client }
    { | requestBuilders }
    { | client' } where
  foldingWithIndex (ClientStep hostInfo reqPrt resDpls) prop c reqBld = do
    let
      sResDpls = Record.get prop resDpls

      subclient = hfoldlWithIndex (ClientStep hostInfo reqPrt sResDpls) {} reqBld

    Record.insert prop subclient c
else instance clientFoldingResponseDuplexNewtypeWrapper ∷
  ( Newtype (f responseDuplexes) responseDuplexes
  , FoldingWithIndex (ClientStep request responseDuplexes) (SProxy sym) { | client } { | requestBuilders } { | client' }
  ) =>
  FoldingWithIndex
    (ClientStep request (f responseDuplexes))
    (SProxy sym)
    { | client }
    { | requestBuilders }
    { | client' } where
  foldingWithIndex (ClientStep hostInfo reqPrt resDpl) prop c reqBld = do
    foldingWithIndex (ClientStep hostInfo reqPrt (unwrap resDpl)) prop c reqBld

-- | Pass a proxy for your nested request `Variant` type to get back
-- | a nested `Record` with functions which builds a given `Variant`.
-- |
-- | This is a bit low level and you probably want to use helpers for
-- | specs which provide this type and proxy for you.
requestBuilders ∷ ∀ requestBuilders t. HFoldlWithIndex (RequestBuildersStep t t) {} (Proxy t) requestBuilders ⇒ Proxy t → requestBuilders
requestBuilders = hfoldlWithIndex (RequestBuildersStep (identity ∷ t → t)) {}

-- | TODO: In this folding we pass response duplex around but we need only response parser.
-- | Perform a recursive heterogeneous map which extracts only parsers and use this cleaner
-- | value for `ClientStep` folding.
client ∷
  ∀ body client requestBuilders responseDuplexes ireq oreq.
  HFoldlWithIndex (RequestBuildersStep ireq ireq) {} (Proxy ireq) requestBuilders ⇒
  HFoldlWithIndex (ClientStep ireq responseDuplexes) {} requestBuilders client ⇒
  HostInfo →
  Request.Duplex body ireq oreq →
  responseDuplexes →
  client
client hostInfo (Request.Duplex reqPrt _) resDpl = do
  let
    rb ∷ requestBuilders
    rb = requestBuilders (Proxy ∷ Proxy ireq)
  hfoldlWithIndex (ClientStep hostInfo reqPrt resDpl) {} rb ∷ client

