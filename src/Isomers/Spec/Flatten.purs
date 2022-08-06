module Isomers.Spec.Flatten where


import Data.Lens (iso)
import Data.Variant (Variant)
import Data.Variant as V
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Contrib.Heterogeneous.Foldings.Record as Foldings.Record
import Isomers.Contrib.Heterogeneous.Foldings.Variant as Foldings.Variant
import Isomers.Spec.Types (Spec(..), Spec')
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Eval (class Eval)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow)
import Type.Prelude (Proxy(..))

flatten
  :: forall flattenHandlers flattenHandlersRL flat flat_ resp resp' respRlOfRl v v_ vl unflattenHandlers unflattenHandlersRL
  -- | Flatten `Variant` constraints
   . RowToList v vl
  => Eval (Foldings.Variant.FoldPrefixRec' "." "" v) flat
  => HFoldlWithIndex (Foldings.Variant.FlattenRecStep "." v flat "") {} (Proxy vl) { | flattenHandlers }
  -- | Required by `Variant` to invoke `match`
  => RL.RowToList flattenHandlers flattenHandlersRL
  => V.VariantMatchCases flattenHandlersRL v_ (Variant flat)
  => R.Union v_ () v


  -- | Unflatten `Variant` constraints
  => HFoldlWithIndex (Foldings.Variant.UnflattenRecStep "." "" v v) {} (Proxy vl) { | unflattenHandlers }

  -- | Required by `Variant` to invoke `match`
  => RL.RowToList unflattenHandlers unflattenHandlersRL
  => V.VariantMatchCases unflattenHandlersRL flat_ (Variant v)
  => R.Union flat_ () flat

  -- | Flatten `Record` constraints
  => Eval ((Map Foldings.Record.ToRowListStep <<< FromRow) resp) respRlOfRl
  => HFoldlWithIndex (Foldings.Record.FlattenRecStep "." resp) {} (Proxy respRlOfRl) { | resp' } -- => { | resp } -> { | resp' }


  => Spec' (Variant v) { | resp }
  -> Spec' (Variant flat) { | resp' }
flatten (Spec { request, response }) = do
  let
    unflattenRequest :: Variant flat -> Variant v
    unflattenRequest = Foldings.Variant.unflatten (Proxy :: Proxy (Variant v))

    request' = iso unflattenRequest Foldings.Variant.flatten  request

    response' = Foldings.Record.flattenRec response
  Spec
    { request: request'
    , response: response'
    }

