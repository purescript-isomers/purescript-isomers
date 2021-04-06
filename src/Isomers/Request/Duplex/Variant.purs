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
  ∀ body i l o lo vi vi' vo vo'.
  IsSymbol l ⇒
  Row.Cons l o vo vo' ⇒
  Row.Cons l i vi vi' ⇒
  Row.Union vo lo vo' ⇒
  SProxy l →
  Duplex body i o →
  Duplex body (Variant vi) (Variant vo) →
  Duplex body (Variant vi') (Variant vo')
injInto l (Duplex prt prs) (Duplex vPrt vPrs) = Duplex vPrt' vPrs'
  where
  vPrt' = vPrt # Variant.on l prt

  vPrs' = Variant.expand <$> vPrs <|> Variant.inj l <$> prs

empty ∷ ∀ body. Duplex body (Variant ()) (Variant ())
empty = Duplex prt prs
  where
    prt ∷ Variant () → Printer
    prt = const mempty

    prs ∷ Parser body (Variant ())
    prs = Parser.Chomp \state → pure $ Parser.Fail $ Parser.Expected "Variant" (unsafeStringify state)

