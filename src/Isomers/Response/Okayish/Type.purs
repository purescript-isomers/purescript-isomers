module Isomers.Response.Okayish.Type
  ( module Exports
  , badRequest
  , _badRequest
  , BadRequest
  , toEither
  , fromEither
  , fromVariant
  , toVariant
  , roundtripEither
  , unsafeFromEither
  , _ok
  , ok
  , Ok
  , Okayish
  , _notFound
  , notFound
  , NotFound
  , _found
  , found
  , Found
  , Location
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Isomers.Contrib.Data.Variant (append) as Contrib.Data.Variant
import Isomers.Response.Duplex (Duplex(..), Duplex') as Exports
import Isomers.Response.Duplex (Duplex)
import Isomers.Response.Encodings (ClientResponse)
import Prim.Row (class Lacks) as Row
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

_ok = Proxy :: Proxy "ok"

-- | Alias useful when working with underling `Variant`.
type Ok a res = (ok :: a | res)

-- | A simple proposition for http response encoding.
-- | You can roll your own if you don't like it
-- | but we provide some convenient helpers here and
-- | in the context of `Spec` build up.
-- |
-- | I've tried to use simple `Either (Variant res) a` here
-- | but it seems that it is a bit harder to extend / compose
-- | this kind of encoders.
newtype Okayish (res :: # Type) a = Okayish (Variant (Ok a + res))

ok :: forall a res. a -> Okayish res a
ok a = Okayish $ Variant.inj _ok a

notOk :: forall a res. Row.Lacks "ok" res => Variant res -> Okayish res a
notOk = Okayish <<< Contrib.Data.Variant.append _ok

fromVariant :: forall a res. Variant (Ok a + res) -> Okayish res a
fromVariant = Okayish

toVariant :: forall a res. Okayish res a -> Variant (Ok a + res)
toVariant (Okayish v) = v

toEither :: forall a res. Okayish res a -> Either (Variant res) a
toEither (Okayish v) = Variant.on _ok Right Left v

fromEither :: forall a res. Row.Lacks "ok" res => Either (Variant res) a -> Okayish res a
fromEither (Right a) = ok a

fromEither (Left v) = notOk v

roundtripEither
  :: forall a b res res'. (Either (Variant res) a -> Either (Variant res') b) -> Okayish res a -> Okayish res' b
roundtripEither f = unsafeFromEither <<< f <<< toEither

unsafeFromEither :: forall a res. Either (Variant res) a -> Okayish res a
unsafeFromEither (Right a) = Okayish $ Variant.inj _ok a

unsafeFromEither (Left v) = Okayish (appendOk v)
  where
  appendOk :: Variant res -> Variant (Ok a + res)
  appendOk = unsafeCoerce

instance functorOkayish :: Functor (Okayish res) where
  map = roundtripEither <<< map

instance applyOkayish :: Apply (Okayish res) where
  apply r1 r2 = roundtripEither (apply (toEither r1)) r2

instance applicativeOkayish :: Applicative (Okayish res) where
  pure = Okayish <<< Variant.inj _ok

instance bindOkayish :: Bind (Okayish res) where
  bind r1 r2 = roundtripEither (\ea -> ea >>= map toEither r2) r1

instance monadOkayish :: Monad (Okayish res)

instance altOkayish :: Alt (Okayish res) where
  alt r1 r2 = roundtripEither (alt (toEither r1)) r2

instance foldableOkayish :: Foldable (Okayish res) where
  foldMap f = foldMap f <<< toEither
  foldr f z = foldr f z <<< toEither
  foldl f z = foldl f z <<< toEither

instance traversableOkayish :: Traversable (Okayish res) where
  traverse f = map unsafeFromEither <<< traverse f <<< toEither
  sequence v = sequenceDefault v

lmapOkayish
  :: forall a res res'
   . Row.Lacks "ok" res'
  => (Variant res -> Variant res')
  -> Okayish res a
  -> Okayish res' a
lmapOkayish f (Okayish v) = Okayish v'
  where
  v' = Variant.on _ok (Variant.inj _ok) (Contrib.Data.Variant.append _ok <<< f) v

_notFound = Proxy :: Proxy "notFound"

type NotFound a res = (notFound :: a | res)

notFound :: forall a b res. Row.Lacks "ok" res => a -> Okayish (NotFound a + res) b
notFound = notOk <<< Variant.inj _notFound

-- | On the client side we are going to have actual response at hand because
-- | we detect redirect post factum (by using `response.redirected`).
_found = Proxy :: Proxy "found"

type Location = String

type Found res = (found :: Location /\ Maybe ClientResponse | res)

found :: forall a res. Row.Lacks "ok" res => Location /\ Maybe ClientResponse -> Okayish (Found + res) a
found = notOk <<< Variant.inj _found

_badRequest = Proxy :: Proxy "badRequest"

type BadRequest a res = (badRequest :: a | res)

badRequest :: forall a b res. Row.Lacks "ok" res => a -> Okayish (BadRequest a + res) b
badRequest = notOk <<< Variant.inj _badRequest

type OkayishDuplex ct vi vo a b = Duplex ct (Okayish vi a) (Okayish vo b)

type OkayishDuplex' ct v a = Duplex ct (Okayish v a) (Okayish v a)

