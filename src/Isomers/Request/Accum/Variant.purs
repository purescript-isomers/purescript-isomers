module Isomers.Request.Accum.Variant where

import Prelude

import Control.Alt ((<|>))
import Data.Variant (Variant)
import Data.Variant (expand, inj, on) as Variant
import Global.Unsafe (unsafeStringify)
import Isomers.Request.Accum.Type (Accum(..))
import Isomers.Request.Duplex.Parser (Parser(..), ParsingError(..), Result(..)) as Parser
import Isomers.Request.Duplex.Parser (Parser)
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Type (Duplex(..))
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (class IsSymbol, SProxy)

injInto ∷
  ∀ body route i l o lo vi vi' vo vo'.
  IsSymbol l ⇒
  Row.Cons l o vo vo' ⇒
  Row.Cons l i vi vi' ⇒
  Row.Union vo lo vo' ⇒
  SProxy l →
  Accum body route i o →
  Accum body route (Variant vi) (Variant vo) →
  Accum body route (Variant vi') (Variant vo')
injInto l (Accum (Duplex prt prs) dst) (Accum (Duplex vPrt vPrs) vDst) = Accum (Duplex vPrt' vPrs') vDst'
  where
  vPrt' = vPrt # Variant.on l prt

  vPrs' = map Variant.expand <$> vPrs <|> compose (Variant.inj l) <$> prs

  vDst' = vDst
    # Variant.on l dst

empty ∷ ∀ body route. Accum body route (Variant ()) (Variant ())
empty = Accum (Duplex prt prs) dst
  where
    prt ∷ Variant () → Printer
    prt = const mempty

    prs ∷ Parser body (route → Variant ())
    prs = Parser.Chomp \state → pure $ Parser.Fail $ Parser.Expected "Empty Variant match" (unsafeStringify state)

    dst _ = unsafeCrashWith $ "Accum.Variant.emtpy"

