module Isomers.Contrib.Heterogeneous.Foldings.Variant where

import Prelude

import Data.Variant (class VariantMatchCases, Variant)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Data.Variant as V
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Contrib.Type.Eval.Semigroup (type (<>))
import Prim.Row (class Cons) as Row
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (RowList)
import Prim.RowList as RL
import Prim.Symbol (class Append) as Symbol
import Prim.Symbol (class Append) as Symbol
import Record (get, insert, union) as Record
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Prelude (class IsSymbol, Proxy(Proxy))

--             AccumPrefixVariant :: separator -> prefix -> label -> type -> tail expr -> expr
foreign import data PrefixRecStep
  :: Symbol -> Symbol -> Symbol -> Type -> TypeExpr (RowList Type) -> TypeExpr (RowList Type)

type FoldPrefixRec separator prefix = FoldrWithIndex (PrefixRecStep separator prefix) (Lift RL.Nil)
type FoldPrefixRec' separator prefix = ToRow <<< FoldrWithIndex (PrefixRecStep separator prefix) (Lift RL.Nil) <<<
  FromRow

instance
  ( Symbol.Append prefix label prefix_
  , Symbol.Append prefix_ separator prefix'

  , Eval (FromRow v) vl
  , Eval ((FoldPrefixRec separator prefix') vl) vRes

  , Eval tail tail'
  , Eval (vRes <> tail') res

  ) =>
  Eval (PrefixRecStep separator prefix label (Variant v) tail) res
else instance
  ( Symbol.Append prefix label prefix'
  , Eval tail tail'
  , IsSymbol prefix'
  ) =>
  Eval (PrefixRecStep separator prefix label value tail) (RowList.Cons prefix' value tail')

type Nested = Variant
  ( a1 ::
      Variant
        ( a2 ::
            Variant
              ( a3 :: Int
              , b3 :: Variant ()
              , d3 :: String
              )
        , b2 ::
            Variant
              ( a3 :: String
              , c3 :: String
              )
        )
  )

flattenExperiment
  :: forall flat v vl. RowToList v vl => Eval (FoldPrefixRec' "." "" v) flat => Proxy (Variant v) -> Proxy flat
flattenExperiment _ = Proxy

x
  :: Proxy
       ( "a1.a2.a3" :: Int
       , "a1.a2.d3" :: String
       , "a1.b2.a3" :: String
       , "a1.b2.c3" :: String
       )
x = flattenExperiment (Proxy :: Proxy Nested)

data FlattenRecStep (sep :: Symbol) (variant :: Row Type) (res :: Row Type) (prefix :: Symbol) = FlattenRecStep
  (Proxy sep)

instance
  ( RowToList v vl
  , Symbol.Append prefix l prefix_
  , Symbol.Append prefix_ sep prefix'
  , HFoldlWithIndex (FlattenRecStep sep v res prefix') {} (Proxy vl) { | handlers }

  -- | Required by `Variant` to invoke `match`
  , RL.RowToList handlers handlersRL
  , V.VariantMatchCases handlersRL v_ (Variant res)
  , R.Union v_ () v

  , IsSymbol l
  , Row.Lacks l acc
  , Row.Cons l (Variant v -> Variant res) acc acc'
  ) =>
  FoldingWithIndex (FlattenRecStep sep variant res prefix) (Proxy l) { | acc } (Proxy (Variant v)) { | acc' } where
  foldingWithIndex (FlattenRecStep sep) l acc _ = do
    let
      step = FlattenRecStep sep :: FlattenRecStep sep v res prefix'
      handlers = hfoldlWithIndex step {} (Proxy :: Proxy vl)
    Record.insert l (V.match handlers) acc
else instance
  ( Symbol.Append prefix l prefix'

  , IsSymbol prefix'
  , Row.Cons prefix' a res_ res

  , IsSymbol l
  , Row.Lacks l acc
  , Row.Cons l (a -> Variant res) acc acc'
  ) =>
  FoldingWithIndex (FlattenRecStep sep variant res prefix) (Proxy l) { | acc } (Proxy a) { | acc' } where
  foldingWithIndex (FlattenRecStep _) l acc _ = do
    Record.insert l ((V.inj (Proxy :: Proxy prefix')) :: a -> Variant res) acc

z
  :: Variant
       ( "a1.a2.a3" :: Int
       , "a1.a2.d3" :: String
       , "a1.b2.a3" :: String
       , "a1.b2.c3" :: String
       )
z = flatten do
  let
    v :: Nested
    v = (V.inj (Proxy :: _ "a1") $ V.inj (Proxy :: _ "b2") $ V.inj (Proxy :: _ "c3") "test")
  v

flatten
  :: forall handlers handlersRL flat v v_ vl
   . RowToList v vl
  => Eval (FoldPrefixRec' "." "" v) flat
  => HFoldlWithIndex (FlattenRecStep "." v flat "") {} (Proxy vl) { | handlers }
  -- | Required by `Variant` to invoke `match`
  => RL.RowToList handlers handlersRL
  => V.VariantMatchCases handlersRL v_ (Variant flat)
  => R.Union v_ () v
  => Variant v
  -> Variant flat
flatten = do
  let
    step = (FlattenRecStep (Proxy :: Proxy ".") :: FlattenRecStep "." v flat "")
  V.match (hfoldlWithIndex step {} (Proxy :: Proxy vl))

data UnflattenRecStep (sep :: Symbol) (prefix :: Symbol) (param :: Row Type) (unflattened :: Row Type) = UnflattenRecStep
  (Variant param -> Variant unflattened)

instance
  ( Symbol.Append prefix l prefix_
  , Symbol.Append prefix_ sep prefix'
  , RowToList child childRL
  , HFoldlWithIndex (UnflattenRecStep sep prefix' child unflattened) { | acc } (Proxy childRL) { | acc' }

  , IsSymbol l
  , Row.Cons l (Variant child) parent_ parent
  ) =>
  FoldingWithIndex (UnflattenRecStep sep prefix parent unflattened)
    (Proxy l)
    { | acc }
    (Proxy (Variant child))
    { | acc' } where
  foldingWithIndex (UnflattenRecStep f) l acc _ = do
    let
      f' :: Variant child -> Variant unflattened
      f' = f <<< V.inj l

    hfoldlWithIndex (UnflattenRecStep f' :: UnflattenRecStep sep prefix' child unflattened) acc (Proxy :: Proxy childRL)
else instance
  ( -- | This is given because we are actually
    -- | folding the actual `parent` here.
    -- | I have to state it so I can use `V.inj`.
    IsSymbol l
  , Row.Cons l a parent_ parent

  -- | Insert the final constructor to the resulting Record
  -- | of handlers.
  , Symbol.Append prefix l prefix'
  , IsSymbol prefix'
  , R.Lacks prefix' acc
  , R.Cons prefix' (a -> Variant unflattened) acc acc'
  ) =>
  FoldingWithIndex (UnflattenRecStep sep prefix parent unflattened) (Proxy l) { | acc } (Proxy a) { | acc' } where
  foldingWithIndex (UnflattenRecStep f) l acc _ = do
    let
      f' :: a -> Variant unflattened
      f' = f <<< V.inj l

      prefix' = Proxy :: Proxy prefix'
    Record.insert prefix' f' acc

unflatten
  :: forall handlers handlersRL flat flat_ unflattened unflattenedRL
   . RowToList unflattened unflattenedRL
  => HFoldlWithIndex (UnflattenRecStep "." "" unflattened unflattened) {} (Proxy unflattenedRL) { | handlers }

  -- | Required by `Variant` to invoke `match`
  => RL.RowToList handlers handlersRL
  => V.VariantMatchCases handlersRL flat_ (Variant unflattened)
  => R.Union flat_ () flat

  => Proxy (Variant unflattened)
  -> Variant flat
  -> Variant unflattened
unflatten _ = do
  let
    step = (UnflattenRecStep identity :: UnflattenRecStep "." "" unflattened unflattened)
  V.match (hfoldlWithIndex step {} (Proxy :: Proxy unflattenedRL))

unflattenNested
  :: Variant
       ( "a1.a2.a3" :: Int
       , "a1.a2.d3" :: String
       , "a1.b2.a3" :: String
       , "a1.b2.c3" :: String
       )
  -> Variant
       ( a1 ::
           Variant
             ( a2 ::
                 Variant
                   ( a3 :: Int
                   , b3 :: Variant ()
                   , d3 :: String
                   )
             , b2 ::
                 Variant
                   ( a3 :: String
                   , c3 :: String
                   )
             )
       )
unflattenNested = unflatten (Proxy :: Proxy Nested)

