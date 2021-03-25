module Isomers.Request.Duplex.Record where

import Prelude

import Isomers.Request.Duplex.Duplex (Duplex(..))
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert) as Record
import Request.Duplex (RequestDuplex(..))
import Type.Prelude (class IsSymbol, SProxy)

insert ∷
  ∀ a l r ro.
  IsSymbol l ⇒
  Row.Lacks l r ⇒
  Row.Cons l a r ro ⇒
  SProxy l →
  RequestDuplex a a →
  Duplex (Record r) (Record ro) (Record ro)
insert l (RequestDuplex prt prs) =
  Duplex (prt <<< Record.get l) (Record.insert l <$> prs)
