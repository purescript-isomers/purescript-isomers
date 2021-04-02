module Isomers.Contrib.Type.Data.Maybe where

import Type.Eval (Lift)

foreign import kind Maybe'

-- | I roll my own because I want to provide foldings / mappings etc.
foreign import data Just' ∷ Type → Maybe'
foreign import data Nothing' ∷ Maybe'

data MProxy (m ∷ Maybe') = MProxy

type JustExpr a = Lift (MProxy (Just' a))
type NothingExpr = Lift (MProxy Nothing')
