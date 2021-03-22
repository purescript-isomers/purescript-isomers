module Isomers.Request.Duplex.Variant where

import Prelude

import Control.Alt ((<|>))
import Data.Variant (Variant)
import Data.Variant (expand, inj) as Variant
import Global.Unsafe (unsafeStringify)
import Isomers.Request.Duplex.Duplex (Duplex(..))
import Prim.Row (class Cons, class Union) as Row
import Request.Duplex.Parser (RequestParser(..), RouteError(..), RouteResult(..)) as Parser
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter)
import Type.Prelude (class IsSymbol, SProxy)


import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant (class Contractable, contract, expand, inj, on) as Variant
import Isomers.Request.Duplex.Duplex (Duplex(..), Duplex')
import Type.Prelude (class IsSymbol, SProxy)

expand ∷
  ∀ i v v_ v'.
  Partial ⇒
  Variant.Contractable v' v ⇒
  Row.Union v v_ v' ⇒
  Duplex' i (Variant v) →
  Duplex' i (Variant v')
expand (Duplex prt prs) = Duplex prt' prs'
  where
  prs' = map Variant.expand <$> prs

  prt' v = case Variant.contract v of
    Just v' → prt v'
    Nothing → mempty

on ∷
  ∀ i l o lo v v'.
  IsSymbol l ⇒
  Row.Cons l o v v' ⇒
  Row.Cons l o () lo ⇒
  Row.Union v lo v' ⇒
  SProxy l →
  Duplex' i o →
  Duplex' i (Variant v) →
  Duplex' i (Variant v')
on l (Duplex prt prs) (Duplex vPrt vPrs) = Duplex vPrt' vPrs'
  where
  vPrt' = vPrt # Variant.on l prt

  vPrs' = (map Variant.expand <$> vPrs) <|> (map (Variant.inj l) <$> prs)

empty ∷ ∀ r. Duplex r (Variant ()) (Variant ())
empty = Duplex prt prs
  where
    prt ∷ Variant () → RequestPrinter
    prt = const mempty

    prs ∷ RequestParser (r → Variant ())
    prs = Parser.Chomp \state → Parser.Fail $ Parser.Expected "Variant" (unsafeStringify state)

