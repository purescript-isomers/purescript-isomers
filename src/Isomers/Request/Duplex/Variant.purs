module Isomers.Request.Duplex.Variant where

import Prelude

import Control.Alt ((<|>))
import Data.Variant (Variant)
import Data.Variant (expand, inj, on) as Variant
import Global.Unsafe (unsafeStringify)
import Isomers.Request.Duplex.Parser (Parser(..), ParsingError(..), Result(..)) as Parser
import Isomers.Request.Duplex.Parser (Parser)
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Type (Duplex(..))
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (class IsSymbol, SProxy)

injInto ∷
  ∀ body i l o lo r vi vi' vo vo'.
  IsSymbol l ⇒
  Row.Cons l o vo vo' ⇒
  Row.Cons l i vi vi' ⇒
  Row.Union vo lo vo' ⇒
  SProxy l →
  Duplex body r i o →
  Duplex body r (Variant vi) (Variant vo) →
  Duplex body r (Variant vi') (Variant vo')
injInto l (Duplex prt prs) (Duplex vPrt vPrs) = Duplex vPrt' vPrs'
  where
  vPrt' = vPrt # Variant.on l prt

  vPrs' = map Variant.expand <$> vPrs <|> map (Variant.inj l) <$> prs

empty ∷ ∀ body r. Duplex body r (Variant ()) (Variant ())
empty = Duplex prt prs
  where
    prt ∷ Variant () → Printer
    prt = const mempty

    prs ∷ Parser body (r → Variant ())
    prs = Parser.Chomp \state → pure $ Parser.Fail $ Parser.Expected "Variant" (unsafeStringify state)

