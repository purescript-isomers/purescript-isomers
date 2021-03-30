module Isomers.Request.Duplex.Record where

import Prelude

import Data.Either (Either)
import Effect.Aff (Fiber)
import Isomers.Request.Duplex.Parser (as, body, int, param, take) as Parser
import Isomers.Request.Duplex.Parser (Parser)
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Printer (body, param, put) as Printer
import Isomers.Request.Duplex.Type (Duplex(..))
import Isomers.Request.Types (ClientBody)
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert) as Record
import Type.Prelude (class IsSymbol, SProxy, reflectSymbol)

type Root body o
  = Duplex body {} { | o } { | o }

insert ∷
  ∀ body i l o r ri ro.
  IsSymbol l ⇒
  Row.Lacks l r ⇒
  Row.Cons l o r ro ⇒
  Row.Cons l i r ri ⇒
  SProxy l →
  (i → Printer) →
  Parser body o →
  Duplex body (Record r) (Record ri) (Record ro)
insert l prt prs = Duplex (prt <<< Record.get l) (Record.insert l <$> prs)

segment ∷
  ∀ t23 t24 t25 t26 t27 t29 t33.
  IsSymbol t27 ⇒
  Row.Lacks t27 t25 ⇒
  Row.Cons t27 t26 t25 t23 ⇒
  Row.Cons t27 t33 t25 t24 ⇒
  SProxy t27 →
  (t33 → String) →
  (String → Either String t26) →
  Duplex t29 (Record t25) (Record t24) (Record t23)
segment l f g = insert l (Printer.put <<< f) (Parser.as identity g Parser.take)

intSegment ∷
  ∀ t79 t80 t82 t83.
  IsSymbol t80 ⇒
  Row.Lacks t80 t82 ⇒
  Row.Cons t80 Int t82 t83 ⇒
  SProxy t80 →
  Duplex t79 (Record t82) (Record t83) (Record t83)
intSegment l = segment l (show ∷ Int → String) Parser.int

param ∷
  ∀ t23 t24 t25 t26 t27 t29 t33.
  IsSymbol t27 ⇒
  Row.Lacks t27 t25 ⇒
  Row.Cons t27 t26 t25 t23 ⇒
  Row.Cons t27 t33 t25 t24 ⇒
  SProxy t27 →
  String →
  (t33 → String) →
  (String → Either String t26) →
  Duplex t29 (Record t25) (Record t24) (Record t23)
param l n f g = insert l (Printer.param n <<< f) (Parser.as identity g $ Parser.param n)

param' ∷
  ∀ t23 t24 t25 t26 t27 t29 t33.
  IsSymbol t27 ⇒
  Row.Lacks t27 t25 ⇒
  Row.Cons t27 t26 t25 t23 ⇒
  Row.Cons t27 t33 t25 t24 ⇒
  SProxy t27 →
  (t33 → String) →
  (String → Either String t26) →
  Duplex t29 (Record t25) (Record t24) (Record t23)
param' l f g = do
  let
    n = reflectSymbol l
  insert l (Printer.param n <<< f) (Parser.as identity g $ Parser.param n)

empty ∷ ∀ body. Duplex body {} {} {}
empty = Duplex (const mempty) (pure identity)

body ∷
  ∀ t100 t101 t102 t104 t105 t106 t107 t96 t99.
  IsSymbol t106 ⇒
  IsSymbol t96 ⇒
  Row.Lacks t106 t101 ⇒
  Row.Cons t106 t102 t101 t99 ⇒
  Row.Cons t106 t104 t101 t100 ⇒
  Row.Cons t96 (Fiber t102) t107 t105 ⇒
  SProxy t96 →
  SProxy t106 →
  (t104 → ClientBody) →
  Duplex t105 (Record t101) (Record t100) (Record t99)
body b l prt = insert l (Printer.body <<< prt) (Parser.body b)
