module Test.Main where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Argonaut (fromNumber, fromString, toNumber, toString) as Argonaut
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Number (fromString) as Number
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant (class Contractable, contract, expand) as Variant
import Data.Variant.Internal (VariantRep(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.Row (class Cons, class Union) as Row
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, kind RowList)
import Prim.Symbol (class Append) as Symbol
import Record (union) as Record
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Routing.Duplex (RouteDuplex', prefix) as Routing.Duplex
import Routing.Duplex (RouteDuplex(..), RouteDuplex')
import Routing.Duplex (int, segment) as Routing.Duplex
import Routing.Duplex.Generic.Variant (Updater, modify, update) as Generic.Variant
import Routing.Duplex.Generic.Variant (class VariantParser, class VariantPrinter)
import Routing.Duplex.Generic.Variant (variant) as Routing.Duplex.Generic.Variant
import Routing.Duplex.Parser (RouteError(..), RouteParser(..), RouteResult(..)) as Duplex.Parser
import Routing.Duplex.Parser (RouteParser)
import Routing.Duplex.Printer (RoutePrinter(..))
import Type.Eval (class Eval)
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..), reflectSymbol)
import Type.Row (RProxy)
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Hybrid.Contrib.Routing.Duplex (unitDuplex)
import WebRow.Hybrid.Contrib.Type.Eval.Tuple (Tuples)
import WebRow.Hybrid.Data.Variant.Prefix (class PrefixRow, class UnprefixRow, add, remove) as Variant.Prefix
import WebRow.Hybrid.Router (Router(..))
import WebRow.Hybrid.Router (duplex, prefix, router) as Router
import WebRow.Hybrid.Router.Run (Response(..), ResponseCodec(..))
import WebRow.Hybrid.Router.Run (route) as Router.Run

-- | Let's define some "codecs" by hand
number ∷ ResponseCodec Number
number =
  ResponseCodec
    { decode: Number.fromString <=< (_.content <<< un Response)
    , encode: \content → Response { statusCode: 200, content: Just (show content) }
    }

string ∷ ResponseCodec String
string =
  ResponseCodec
    { decode: _.content <<< un Response
    , encode: \content → Response { statusCode: 200, content: Just content }
    }

-- | Type signature is fully derived and optional here.
router ::
  Router
    ( "admin.dashboard" :: ResponseCodec Number
    , "admin.profile" :: ResponseCodec String
    )
    ( "admin.dashboard" :: Tuple Int Unit
    , "admin.profile" :: Tuple Int Int
    )
router =
  Router.duplex (Routing.Duplex.int Routing.Duplex.segment)
    $ Router.prefix (SProxy ∷ SProxy ".")
        { admin:
            Router.router true
              { dashboard: unitDuplex Routing.Duplex.segment /\ number
              , profile: Routing.Duplex.int Routing.Duplex.segment /\ string
              }
        }

-- route ::
--   forall t216.
--   Either (Request String)
--     ( Variant
--         ( "admin.dashboard" :: Tuple Int Unit
--         , "admin.profile" :: Tuple Int Int
--         )
--     ) ->
--   Run
--     ( "admin.dashboard" :: FProxy (FetchF (Tuple Int Unit) Number)
--     , "admin.profile" :: FProxy (FetchF (Tuple Int Int) String)
--     , routeNotFound :: FProxy (Except (Tuple RouteError (Request String)))
--     | t216
--     )
--     String
route =
  Router.Run.route router
    { "admin.dashboard": const $ pure "TEST"
    , "admin.profile": const $ pure "TEST"
    }
