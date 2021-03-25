module Isomers.Response
  ( module DuplexE
  , injInto
  , toEither
  , fromEither
  , roundtripEither
  , unsafeFromEither
  , Response
  , _ok
  , ok
  , Ok
  , json
  , _notFound
  , notFound
  , notFound'
  , NotFound
  , _found
  , found
  , Found
  , _movedPermanently
  , movedPermanently
  , MovedPermanently
  , wrapVariantDuplex
  , unwrapToVariantDuplex
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lens (iso)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Isomers.Contrib.Data.Variant (append) as Contrib.Data.Variant
import Isomers.HTTP.ContentTypes (_json)
import Isomers.Response.Duplex (Duplex(..)) as DuplexE
import Isomers.Response.Duplex (json, reqHeader, withHeaderValue, withStatus) as Duplex
import Isomers.Response.Duplex (Duplex, withStatus)
import Isomers.Response.Duplex.Variant (empty, injInto) as Duplex.Variant
import Network.HTTP.Types (found302, hContentType, hLocation, movedPermanently301, notFound404, ok200)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

_ok = SProxy ∷ SProxy "ok"

-- | Alias useful when working with underling `Variant`.
type Ok a res
  = ( ok ∷ a | res )

-- | A simple proposition for http response encoding.
-- | You can roll your own if you don't like it
-- | but we provide some convenient helpers here and
-- | in the context of `Spec` build up.
-- |
-- | I've tried to use simple `Either (Variant res) a` here
-- | but it seems that it is a bit harder to extend / compose
-- | this kind of encoders.
newtype Response (contentType ∷ Symbol) (res ∷ # Type) a
  = Response (Variant (Ok a + res))

fromVariant ∷ ∀ a contentType res. SProxy contentType → Variant (Ok a + res) → Response contentType res a
fromVariant ct v = Response v

toVariant ∷ ∀ a contentType res. Response contentType res a → Variant (Ok a + res)
toVariant (Response v) = v

toEither ∷ ∀ a ct res. Response ct res a → Either (Variant res) a
toEither (Response v) = Variant.on _ok Right Left v

fromEither ∷ ∀ a ct res. Row.Lacks "ok" res ⇒ Row.Cons "ok" a res (Ok a + res) ⇒ SProxy ct → Either (Variant res) a → Response ct res a
fromEither _ (Right a) = Response $ Variant.inj _ok a

fromEither _ (Left v) = Response $ (append' v)
  where
  append' ∷ Variant res → Variant (Ok a + res)
  append' = Contrib.Data.Variant.append _ok

-- | Really useful in the context of "manual instances deriving"
roundtripEither ∷ ∀ a b ct res res'. (Either (Variant res) a → Either (Variant res') b) → Response ct res a → Response ct res' b
roundtripEither f = unsafeFromEither (SProxy ∷ SProxy ct) <<< f <<< toEither

unsafeFromEither ∷ ∀ a ct res. SProxy ct → Either (Variant res) a → Response ct res a
unsafeFromEither _ (Right a) = Response $ Variant.inj _ok a

unsafeFromEither _ (Left v) = Response (appendOk v)
  where
  appendOk ∷ Variant res → Variant (Ok a + res)
  appendOk = unsafeCoerce

instance functorResponse ∷ Functor (Response ct res) where
  map = roundtripEither <<< map

instance applyResponse ∷ Apply (Response ct res) where
  apply r1 r2 = roundtripEither (apply (toEither r1)) r2

instance applicativeResponse ∷ Applicative (Response ct res) where
  pure = Response <<< Variant.inj _ok

instance bindResponse ∷ Bind (Response content res) where
  bind r1 r2 = roundtripEither (\ea → ea >>= map toEither r2) r1

instance monadResponse ∷ Monad (Response ct res)

instance altResponse ∷ Alt (Response ct res) where
  alt r1 r2 = roundtripEither (alt (toEither r1)) r2

instance foldableResponse ∷ Foldable (Response ct res) where
  foldMap f = foldMap f <<< toEither
  foldr f z = foldr f z <<< toEither
  foldl f z = foldl f z <<< toEither

instance traversableResponse ∷ Traversable (Response ct res) where
  traverse f = map (unsafeFromEither (SProxy ∷ SProxy ct)) <<< traverse f <<< toEither
  sequence v = sequenceDefault v

lmapResponse ∷
  ∀ a contentType res res'.
  Row.Lacks "ok" res' ⇒
  (Variant res → Variant res') →
  Response contentType res a →
  Response contentType res' a
lmapResponse f (Response v) = Response v'
  where
  v' = Variant.on _ok (Variant.inj _ok) (Contrib.Data.Variant.append _ok <<< f) v

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
  Duplex i o →
  Duplex (Response ct vi a) (Response ct vo b) →
  Duplex (Response ct vi' a) (Response ct vo' b)
injInto l o v = do
  wrapVariantDuplex (Duplex.Variant.injInto l o (unwrapToVariantDuplex v))

wrapVariantDuplex ∷ ∀ a b ct vi vo. Duplex (Variant (ok ∷ a | vi)) (Variant (ok ∷ b | vo)) → Duplex (Response ct vi a) (Response ct vo b)
wrapVariantDuplex = iso toVariant (fromVariant (SProxy ∷ SProxy ct))

unwrapToVariantDuplex ∷ ∀ a b ct vi vo. Duplex (Response ct vi a) (Response ct vo b) → Duplex (Variant (ok ∷ a | vi)) (Variant (ok ∷ b | vo))
unwrapToVariantDuplex = iso (fromVariant (SProxy ∷ SProxy ct)) toVariant

-- | The baseline of our composition
-- | TODO: This is very naive handling of content type
-- | as we could expect charset specification or
-- | more complicated scenarios like handling
-- | multipart content and this header
-- | should contain boundry separator.
ok ∷ ∀ ct i o. IsSymbol ct ⇒ SProxy ct → Duplex i o → Duplex (Response ct () i) (Response ct () o)
ok ct d = wrapVariantDuplex $ Duplex.Variant.injInto _ok (withStatus ok200 $ Duplex.withHeaderValue hContentType (reflectSymbol ct) $ d) Duplex.Variant.empty

json ∷ Duplex (Response "application/json" () Json) (Response "application/json" () Json)
json = ok _json Duplex.json

type Found res
  = ( found ∷ String | res )

_found = SProxy ∷ SProxy "found"

found ∷
  ∀ ct res o i.
  Row.Union res (Found + ()) (Found + res) ⇒
  Duplex (Response ct res i) (Response ct res o) →
  Duplex
    (Response ct (Found + res) i)
    (Response ct (Found + res) o)
found res =
  res
    # injInto _found
        (Duplex.withStatus found302 $ Duplex.reqHeader hLocation)

type MovedPermanently res
  = ( movedPermanently ∷ String | res )

_movedPermanently = SProxy ∷ SProxy "movedPermanently"

movedPermanently ∷
  ∀ ct res o i.
  Row.Union res (MovedPermanently + ()) (MovedPermanently + res) ⇒
  Duplex (Response ct res i) (Response ct res o) →
  Duplex
    (Response ct (MovedPermanently + res) i)
    (Response ct (MovedPermanently + res) o)
movedPermanently res =
  res
    # injInto _movedPermanently
        (Duplex.withStatus movedPermanently301 $ Duplex.reqHeader hLocation)

_notFound = SProxy ∷ SProxy "notFound"

type NotFound a res
  = ( notFound ∷ a | res )

notFound ∷
  ∀ ct res nfi nfo o i.
  Row.Union res (NotFound nfi + ()) (NotFound nfi + res) ⇒
  Row.Union res (NotFound nfo + ()) (NotFound nfo + res) ⇒
  Duplex nfi nfo →
  Duplex (Response ct res i) (Response ct res o) →
  Duplex
    (Response ct (NotFound nfi + res) i)
    (Response ct (NotFound nfo + res) o)
notFound nf res =
  res
    # injInto _notFound
        (Duplex.withStatus notFound404 $ nf)

notFound' ∷
  ∀ i ct o res.
  Row.Union res (NotFound Unit + ()) (NotFound Unit + res) ⇒
  Row.Union res (NotFound Unit + ()) (NotFound Unit + res) ⇒
  Duplex (Response ct res i) (Response ct res o) →
  Duplex
    (Response ct (NotFound Unit + res) i)
    (Response ct (NotFound Unit + res) o)
notFound' res = notFound (pure unit) res

