module Isomers.Response.Okayish.Duplexes where

import Prelude

import Control.Alt (class Alt, alt)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lens (Iso', iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Effect.Aff (Aff)
import Isomers.Contrib.Data.Variant (append) as Contrib.Data.Variant
import Isomers.HTTP.ContentTypes (JsonMime, HtmlMime, _html, _json)
import Isomers.Response.Duplex (Duplex(..), Duplex') as Exports
import Isomers.Response.Duplex (Duplex(..), Duplex', withStatus)
import Isomers.Response.Duplex (asJson, header, json, reqHeader, withHeaderValue, withStatus) as Duplex
import Isomers.Response.Duplex.Parser (ParsingError)
import Isomers.Response.Duplex.Parser (run, string) as Parser
import Isomers.Response.Duplex.Printer (run, string) as Printer
import Isomers.Response.Duplex.Variant (empty, injInto) as Duplex.Variant
import Isomers.Response.Encodings (ClientResponse, ServerResponse) as Encodings
import Isomers.Response.Okayish.Type (Okayish(..), Ok, _ok, fromVariant, toVariant)
import Isomers.Response.Types (HtmlString(..))
import Network.HTTP.Types (found302, hContentType, hLocation, movedPermanently301, notFound404, ok200)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)


type OkayishDuplex ct vi vo i o = Duplex ct (Okayish vi i) (Okayish vo o)

type OkayishDuplex' ct v a = Duplex' ct (Okayish v a)

-- | We assume here that a given duplex was created
-- | by some smart constructor which enforced
-- | content type header.
injInto ∷
  ∀ a b ct l i o li lo vi vi' vo vo'.
  IsSymbol l ⇒
  Row.Cons l o () lo ⇒
  Row.Union vo lo vo' ⇒
  Row.Cons l o (Ok b + vo) (Ok b + vo') ⇒
  Row.Cons l i () li ⇒
  Row.Union vi li vi' ⇒
  Row.Cons l i (Ok a + vi) (Ok a + vi') ⇒
  SProxy l →
  Duplex ct i o →
  Duplex ct (Okayish vi a) (Okayish vo b) →
  Duplex ct (Okayish vi' a) (Okayish vo' b)
injInto l o v = do
  wrapVariantDuplex (Duplex.Variant.injInto l o (unwrapToVariantDuplex v))

-- | Sometimes responses can't be encoded in the same way like
-- | for example image providing HTTP endpoint can return
-- | 404 response which is not an image.
injStrangerInto ∷
  ∀ a b ct ct' l i o li lo vi vi' vo vo'.
  IsSymbol l ⇒
  Row.Cons l o () lo ⇒
  Row.Union vo lo vo' ⇒
  Row.Cons l o (Ok b + vo) (Ok b + vo') ⇒
  Row.Cons l i () li ⇒
  Row.Union vi li vi' ⇒
  Row.Cons l i (Ok a + vi) (Ok a + vi') ⇒
  SProxy l →
  Duplex ct' i o →
  Duplex ct (Okayish vi a) (Okayish vo b) →
  Duplex ct (Okayish vi' a) (Okayish vo' b)
injStrangerInto l o v = do
  wrapVariantDuplex (Duplex.Variant.injInto l (coerceContentType o) (unwrapToVariantDuplex v))
  where
    coerceContentType ∷ Duplex ct' i o → Duplex ct i o
    coerceContentType = unsafeCoerce

wrapVariantDuplex ∷ ∀ a b ct vi vo. Duplex ct (Variant ( Ok a + vi )) (Variant ( Ok b + vo )) → Duplex ct (Okayish vi a) (Okayish vo b)
wrapVariantDuplex = iso toVariant fromVariant

unwrapToVariantDuplex ∷ ∀ a b ct vi vo. Duplex ct (Okayish vi a) (Okayish vo b) → Duplex ct (Variant ( Ok a + vi )) (Variant ( Ok b + vo ))
unwrapToVariantDuplex = iso fromVariant toVariant

-- | The baseline of our composition
-- | TODO: This is very naive handling of content type
-- | as we could expect charset specification or
-- | more complicated scenarios like handling
-- | multipart content and this header
-- | should contain boundry separator.
ok ∷ ∀ ct i o. Duplex ct i o → Duplex ct (Okayish () i) (Okayish () o)
ok d = wrapVariantDuplex $ Duplex.Variant.injInto _ok (withStatus ok200 d) Duplex.Variant.empty

json ∷ Duplex' JsonMime (Okayish () Json)
json = ok Duplex.json

html ∷ Duplex' HtmlMime (Okayish () HtmlString)
html = ok d
  where
  _Method ∷ Iso' HtmlString String
  _Method = _Newtype

  d = _Method (Duplex Printer.string Parser.string)

asJson ∷ ∀ t144 t145. (t145 → Json) → (Json → Either String t144) → Duplex JsonMime (Okayish () t145) (Okayish () t144)
asJson f g = ok $ Duplex.asJson f g

-- type Found res
--   = ( found ∷ String | res )
-- 
-- _found = SProxy ∷ SProxy "found"
-- 
-- found ∷
--   ∀ ct res o i.
--   Row.Union res (Found + ()) (Found + res) ⇒
--   Duplex (Okayish res i) (Okayish res o) →
--   Duplex
--     (Okayish (Found + res) i)
--     (Okayish (Found + res) o)
-- found res =
--   res
--     # injInto _found
--         (Duplex.withStatus found302 $ Duplex.reqHeader hLocation)
-- 
-- type MovedPermanently res
--   = ( movedPermanently ∷ String | res )
-- 
-- _movedPermanently = SProxy ∷ SProxy "movedPermanently"
-- 
-- movedPermanently ∷
--   ∀ ct res o i.
--   Row.Union res (MovedPermanently + ()) (MovedPermanently + res) ⇒
--   Duplex (Okayish res i) (Okayish res o) →
--   Duplex
--     (Okayish (MovedPermanently + res) i)
--     (Okayish (MovedPermanently + res) o)
-- movedPermanently res =
--   res
--     # injInto _movedPermanently
--         (Duplex.withStatus movedPermanently301 $ Duplex.reqHeader hLocation)
-- 
-- _notFound = SProxy ∷ SProxy "notFound"
-- 
-- type NotFound a res
--   = ( notFound ∷ a | res )
-- 
-- notFound ∷
--   ∀ ct res nfi nfo o i.
--   Row.Union res (NotFound nfi + ()) (NotFound nfi + res) ⇒
--   Row.Union res (NotFound nfo + ()) (NotFound nfo + res) ⇒
--   Duplex nfi nfo →
--   Duplex (Okayish res i) (Okayish res o) →
--   Duplex
--     (Okayish (NotFound nfi + res) i)
--     (Okayish (NotFound nfo + res) o)
-- notFound nf res =
--   res
--     # injInto _notFound
--         (Duplex.withStatus notFound404 $ nf)
-- 
-- notFound' ∷
--   ∀ i ct o res.
--   Row.Union res (NotFound Unit + ()) (NotFound Unit + res) ⇒
--   Row.Union res (NotFound Unit + ()) (NotFound Unit + res) ⇒
--   Duplex (Okayish res i) (Okayish res o) →
--   Duplex
--     (Okayish (NotFound Unit + res) i)
--     (Okayish (NotFound Unit + res) o)
-- notFound' res = notFound (pure unit) res

