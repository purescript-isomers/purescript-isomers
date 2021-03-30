module Isomers.Request.Duplex
  ( module Type
  , module Parser
  , module Printer
  ) where

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Lazy (class Lazy)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lazy (Lazy)
import Data.Lazy (force) as Lazy
import Data.Lazy as Z
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple as Tuple
import Data.Variant (Variant)
import Effect.Aff (Aff, Fiber)
import Isomers.Request.Duplex.Parser (Parser(..), body) as Parser
import Isomers.Request.Duplex.Path (Parts, parse) as Path
import Isomers.Request.Duplex.Printer (Printer(..))
import Isomers.Request.Duplex.Printer (Printer(..)) as Printer
import Isomers.Request.Duplex.Type (Duplex(..))
import Isomers.Request.Duplex.Type (Duplex(..), Duplex', parse', print) as Type
import Isomers.Request.Types (ServerRequest)
import Network.HTTP.Types (HeaderName)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy(..))


body ∷ ∀ t3 t4 t5 t6 t7 t8. IsSymbol t7 ⇒ Row.Cons t7 (Fiber (t5 → t3)) t8 t6 ⇒ SProxy t7 → (t4 → Printer) → Duplex t6 t5 t4 t3
body l prt = Duplex prt (Parser.body l)

