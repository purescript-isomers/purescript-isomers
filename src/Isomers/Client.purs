module Isomers.Client where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Effect.Aff (Aff)
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Isomers.Client.Fetch (fetch)
import Isomers.HTTP.Exchange (Error(..)) as Exchange
import Isomers.Request (Accum(..), Duplex(..), Printer) as Request
import Isomers.Request.Duplex.Printer (run) as Request.Duplex.Printer
import Isomers.Response (Duplex, parse) as Response
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList)
import Record (get, insert) as Record
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), SProxy(..))
import Type.Row (type (+))

-- | This folding creates a request builder:
-- | a record which contains functions which put a
-- | request value (nested Variant) togheter based
-- | on the labels from the path.
data RequestBuildersStep a request
  = RequestBuildersStep (a → request)

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
  = ClientStep (request → Request.Printer) responseDuplexes

_exchangeError = SProxy ∷ SProxy "exchangeError"

type ExchangeError err = (exchangeError ∷ Exchange.Error | err)

exchangeError :: forall errs. Exchange.Error -> Variant (ExchangeError + errs)
exchangeError = Variant.inj _exchangeError

newtype ResponseM errs a = ResponseM (ExceptV (ExchangeError + errs) Aff a)

runResponseM :: forall t4 t5.  ResponseM t4 t5 -> Aff (Either (Variant ( ExchangeError + t4)) t5)
runResponseM (ResponseM e) = runExceptT e

-- | TODO: parameterize the client by fetching function
-- | so the whole exchange can be performed purely.
instance clientFoldingResponseConstructor ∷
  ( IsSymbol sym
  , Row.Lacks sym client
  , Row.Cons sym (Response.Duplex ct i res) resDpls_ resDpls
  , Row.Cons sym (d → ResponseM errs res) client client'
  ) =>
  FoldingWithIndex
    (ClientStep request { | resDpls })
    (SProxy sym)
    { | client }
    (d → request)
    { | client' } where
  foldingWithIndex (ClientStep reqPrt resDpls) prop c reqBld = do
    let
      resDpl = Record.get prop resDpls
      f d = ResponseM $ ExceptT do
        let r = reqBld d
        res ← fetch (Request.Duplex.Printer.run (reqPrt r)) >>= case _ of
          Right res → lmap (exchangeError <<< Exchange.Error <<< unsafeStringify) <$> Response.parse resDpl res
          Left err → pure $ Left $ exchangeError $ Exchange.Error err
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
  foldingWithIndex (ClientStep reqPrt resDpls) prop c reqBld = do
    let
      sResDpls = Record.get prop resDpls

      subclient = hfoldlWithIndex (ClientStep reqPrt sResDpls) {} reqBld

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
  foldingWithIndex (ClientStep reqPrt resDpl) prop c reqBld = do
    foldingWithIndex (ClientStep reqPrt (unwrap resDpl)) prop c reqBld

requestBuilders ∷ ∀ requestBuilders t. HFoldlWithIndex (RequestBuildersStep t t) {} (Proxy t) { | requestBuilders } ⇒ Proxy t → { | requestBuilders }
requestBuilders = hfoldlWithIndex (RequestBuildersStep (identity ∷ t → t)) {}

-- | TODO: In this folding we pass response duplex around but we need only response parser.
-- | Perform a recursive heterogeneous map which extracts only parsers and use this cleaner
-- | value for `ClientStep` folding.
client ∷
  ∀ body client requestBuilders responseDuplexes request route rl.
  RowToList requestBuilders rl ⇒
  HFoldlWithIndex (RequestBuildersStep request request) {} (Proxy request) { | requestBuilders } ⇒
  HFoldlWithIndex (ClientStep request responseDuplexes) {} { | requestBuilders } { | client } ⇒
  Request.Accum body route request request →
  responseDuplexes →
  { | client }
client (Request.Accum (Request.Duplex reqPrt _) _) resDpl = do
  let
    rb ∷ { | requestBuilders }
    rb = requestBuilders (Proxy ∷ Proxy request)
  hfoldlWithIndex (ClientStep reqPrt resDpl) {} rb ∷ { | client }

