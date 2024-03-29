module Isomers.Request.Duplex.Variant where

import Prelude

import Control.Alt ((<|>))
import Data.Variant (Variant)
import Data.Variant (expand, inj, on) as Variant
import Isomers.Request.Duplex.Parser (Parser(..), ParsingError(..), Result(..)) as Parser
import Isomers.Request.Duplex.Parser (Parser)
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Type (Duplex(..))
import JS.Unsafe.Stringify (unsafeStringify)
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (class IsSymbol, Proxy)

injInto
  :: forall i l o lo vi vi' vo vo'
   . IsSymbol l
  => Row.Cons l o vo vo'
  => Row.Cons l i vi vi'
  => Row.Union vo lo vo'
  => Proxy l
  -> Duplex i o
  -> Duplex (Variant vi) (Variant vo)
  -> Duplex (Variant vi') (Variant vo')
injInto l (Duplex prt prs) (Duplex vPrt vPrs) = Duplex vPrt' vPrs'
  where
  vPrt' = vPrt # Variant.on l prt

  vPrs' = Variant.expand <$> vPrs <|> Variant.inj l <$> prs

empty :: Duplex (Variant ()) (Variant ())
empty = Duplex prt prs
  where
  prt :: Variant () -> Printer
  prt = const mempty

  prs :: Parser (Variant ())
  prs = Parser.Chomp \state -> pure $ Parser.Fail $ Parser.Expected "Variant" (unsafeStringify state)

