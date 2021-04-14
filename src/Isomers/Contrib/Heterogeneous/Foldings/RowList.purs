module Isomers.Contrib.Heterogeneous.Foldings.RowList where

import Prim.Boolean (False, True, kind Boolean)
import Prim.RowList (Nil) as RL
import Prim.RowList (kind RowList)

class Null (rl ∷ RowList) (b ∷ Boolean) | rl → b

instance nullNil ∷ Null RL.Nil True
else instance nullCons ∷ Null l False

