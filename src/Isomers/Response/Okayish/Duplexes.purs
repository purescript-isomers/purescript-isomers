module Isomers.Response.Okayish.Duplexes where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lens (Iso', iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Effect.Aff (Aff)
import Isomers.Contrib.Data.Variant (append) as Contrib.Data.Variant
import Isomers.HTTP.ContentTypes (HtmlMime, JavascriptMime, JsonMime, JpegMime, _html, _json)
import Isomers.Response.Duplex (Duplex(..), Duplex') as Exports
import Isomers.Response.Duplex (Duplex(..), Duplex', withStatus)
import Isomers.Response.Duplex (asJson, header, json, reqHeader, withHeaderValue, withStatus) as Duplex
import Isomers.Response.Duplex.Parser (ParsingError(..), redirected, run, status, string, untouchedResponse, url) as Parser
import Isomers.Response.Duplex.Parser (ParsingError, untouchedResponse)
import Isomers.Response.Duplex.Printer (reqHeader) as Duplex.Printer
import Isomers.Response.Duplex.Printer (run, string) as Printer
import Isomers.Response.Duplex.Variant (empty, injInto) as Duplex.Variant
import Isomers.Response.Encodings (ClientResponse, ServerResponse) as Encodings
import Isomers.Response.Okayish.Type (BadRequest, Found, NotFound, Ok, Okayish(..), Location, _badRequest, _found, _notFound, _ok, fromVariant, toVariant)
import Isomers.Response.Types (HtmlString(..), JavascriptString(..))
import Network.HTTP.Types (badRequest400, found302, hContentType, hLocation, movedPermanently301, notFound404, ok200)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type OkayishDuplex ct v i o
  = Duplex ct (Okayish v i) (Okayish v o)

type OkayishDuplex' ct v a
  = Duplex' ct (Okayish v a)

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
  Proxy l →
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
  Proxy l →
  Duplex ct' i o →
  Duplex ct (Okayish vi a) (Okayish vo b) →
  Duplex ct (Okayish vi' a) (Okayish vo' b)
injStrangerInto l o v = do
  wrapVariantDuplex (Duplex.Variant.injInto l (coerceContentType o) (unwrapToVariantDuplex v))
  where
  coerceContentType ∷ Duplex ct' i o → Duplex ct i o
  coerceContentType = unsafeCoerce

wrapVariantDuplex ∷ ∀ a b ct vi vo. Duplex ct (Variant (Ok a + vi)) (Variant (Ok b + vo)) → Duplex ct (Okayish vi a) (Okayish vo b)
wrapVariantDuplex = iso toVariant fromVariant

unwrapToVariantDuplex ∷ ∀ a b ct vi vo. Duplex ct (Okayish vi a) (Okayish vo b) → Duplex ct (Variant (Ok a + vi)) (Variant (Ok b + vo))
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

javascript ∷ Duplex' JavascriptMime (Okayish () JavascriptString)
javascript = ok $ _Newtype (Duplex Printer.string Parser.string)

notFound ∷
  ∀ t105 t107 t110 t111 t113 t114 t115.
  Row.Union t105 (NotFound t110 + ()) (NotFound t110 + t105) ⇒
  Row.Union t107 (NotFound t111 + ()) (NotFound t111 + t107) ⇒
  Duplex t113 t111 t110 →
  Duplex t113 (Okayish t107 t115) (Okayish t105 t114) →
  Duplex t113
    (Okayish (NotFound t111 + t107) t115)
    (Okayish (NotFound t110 + t105) t114)
notFound nf res = injInto _notFound (Duplex.withStatus notFound404 $ nf) res

notFound' nf res = injStrangerInto _notFound (Duplex.withStatus notFound404 $ nf) res

foundDuplex :: forall t168. Duplex' t168 (Location /\ Maybe Encodings.ClientResponse)
foundDuplex = Duplex
  (Duplex.Printer.reqHeader hLocation <<< fst)
  prs
  where
    prs = do
      Parser.redirected >>= if _
        then do
          location ← Parser.url
          clientResponse ← Parser.untouchedResponse
          pure $ location /\ Just clientResponse
        else do
          s ← Parser.status
          throwError $ Parser.Expected "redirect" (show s)

found ∷
  ∀ t105 t107 t110 t111 t113 t114 t115.
  Row.Union t105 (Found + ()) (Found + t105) ⇒
  Row.Union t107 (Found + ()) (Found + t107) ⇒
  Duplex t113 (Okayish t107 t115) (Okayish t105 t114) →
  Duplex t113
    (Okayish (Found + t107) t115)
    (Okayish (Found + t105) t114)
found res = injInto _found (Duplex.withStatus found302 $ foundDuplex) res

found' res = injStrangerInto _found (Duplex.withStatus found302 $ foundDuplex) res

badRequest ∷
  ∀ t105 t107 t110 t111 t113 t114 t115.
  Row.Union t105 (BadRequest t110 + ()) (BadRequest t110 + t105) ⇒
  Row.Union t107 (BadRequest t111 + ()) (BadRequest t111 + t107) ⇒
  Duplex t113 t111 t110 →
  Duplex t113 (Okayish t107 t115) (Okayish t105 t114) →
  Duplex t113
    (Okayish ( BadRequest t111 + t107) t115)
    (Okayish ( BadRequest t110 + t105) t114)
badRequest nf res = injInto _badRequest (Duplex.withStatus badRequest400 $ nf) res

badRequest' nf res = injStrangerInto _badRequest (Duplex.withStatus badRequest400 $ nf) res

-- type MovedPermanently res
--   = ( movedPermanently ∷ String | res )
-- 
-- _movedPermanently = Proxy ∷ Proxy "movedPermanently"
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
