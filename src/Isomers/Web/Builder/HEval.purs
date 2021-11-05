module Isomers.Web.Builder.HEval where

import Heterogeneous.Mapping (class Mapping)
import Isomers.Contrib.Heterogeneous.Foldings.RowList (class Null) as Contrib.RowList
import Isomers.Contrib.Heterogeneous.HEval (class HEval)
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..), HNothing)
import Prim.Boolean (False, True)
import Prim.RowList (class RowToList)
import Type.Prelude (Proxy(..))

data DoNull = DoNull

instance hevalDoNull ∷ (RowToList r rl, Contrib.RowList.Null rl b) ⇒ HEval DoNull ({ | r } → Proxy b) where
  heval _ _ = Proxy ∷ Proxy b

class IsHJust m (b ∷ Boolean) | m → b

instance isHJust ∷ IsHJust (HJust a) True
else instance isHJustNothing ∷ IsHJust HNothing False

data DoIsHJust = DoIsHJust

instance hevalDoIsHJust ∷ IsHJust i b ⇒ HEval DoIsHJust (i → Proxy b) where
  heval _ _ = Proxy ∷ Proxy b

data FromHJust = FromHJust

instance mappingFromHJust ∷ Mapping FromHJust (HJust a) a where
  mapping _ (HJust a) = a
