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
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Variant (Variant)
import Data.Variant (inj, on) as Variant
import Isomers.Contrib.Data.Variant (append) as Contrib.Data.Variant
import Isomers.Response.Duplex (Duplex(..), Duplex') as Exports
import Isomers.Response.Duplex (Duplex)
import Prim.Row (class Lacks) as Row
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

_ok = SProxy ∷ SProxy "ok"

-- | Alias useful when working with underling `Variant`.
type Ok a res
  = ( ok ∷ a | res )

ok :: forall a res. a -> Okayish res a
ok a = Okayish $ Variant.inj _ok a

notOk ∷ ∀ a res. Row.Lacks "ok" res ⇒ Variant res → Okayish res a
notOk = Okayish <<< Contrib.Data.Variant.append _ok

type OkayishDuplex ct vi vo a b
  = Duplex ct (Okayish vi a) (Okayish vo b)

type OkayishDuplex' ct v a
  = Duplex ct (Okayish v a) (Okayish v a)

-- | A simple proposition for http response encoding.
-- | You can roll your own if you don't like it
-- | but we provide some convenient helpers here and
-- | in the context of `Spec` build up.
-- |
-- | I've tried to use simple `Either (Variant res) a` here
-- | but it seems that it is a bit harder to extend / compose
-- | this kind of encoders.
newtype Okayish (res ∷ # Type) a
  = Okayish (Variant (Ok a + res))

fromVariant ∷ ∀ a res. Variant (Ok a + res) → Okayish res a
fromVariant = Okayish

toVariant ∷ ∀ a res. Okayish res a → Variant (Ok a + res)
toVariant (Okayish v) = v

toEither ∷ ∀ a res. Okayish res a → Either (Variant res) a
toEither (Okayish v) = Variant.on _ok Right Left v

fromEither ∷ ∀ a res. Row.Lacks "ok" res ⇒ Either (Variant res) a → Okayish res a
fromEither (Right a) = ok a

fromEither (Left v) = notOk v

roundtripEither ∷ ∀ a b res res'. (Either (Variant res) a → Either (Variant res') b) → Okayish res a → Okayish res' b
roundtripEither f = unsafeFromEither <<< f <<< toEither

unsafeFromEither ∷ ∀ a res. Either (Variant res) a → Okayish res a
unsafeFromEither (Right a) = Okayish $ Variant.inj _ok a

unsafeFromEither (Left v) = Okayish (appendOk v)
  where
  appendOk ∷ Variant res → Variant (Ok a + res)
  appendOk = unsafeCoerce

instance functorOkayish ∷ Functor (Okayish res) where
  map = roundtripEither <<< map

instance applyOkayish ∷ Apply (Okayish res) where
  apply r1 r2 = roundtripEither (apply (toEither r1)) r2

instance applicativeOkayish ∷ Applicative (Okayish res) where
  pure = Okayish <<< Variant.inj _ok

instance bindOkayish ∷ Bind (Okayish res) where
  bind r1 r2 = roundtripEither (\ea → ea >>= map toEither r2) r1

instance monadOkayish ∷ Monad (Okayish res)

instance altOkayish ∷ Alt (Okayish res) where
  alt r1 r2 = roundtripEither (alt (toEither r1)) r2

instance foldableOkayish ∷ Foldable (Okayish res) where
  foldMap f = foldMap f <<< toEither
  foldr f z = foldr f z <<< toEither
  foldl f z = foldl f z <<< toEither

instance traversableOkayish ∷ Traversable (Okayish res) where
  traverse f = map unsafeFromEither <<< traverse f <<< toEither
  sequence v = sequenceDefault v

lmapOkayish ∷
  ∀ a res res'.
  Row.Lacks "ok" res' ⇒
  (Variant res → Variant res') →
  Okayish res a →
  Okayish res' a
lmapOkayish f (Okayish v) = Okayish v'
  where
  v' = Variant.on _ok (Variant.inj _ok) (Contrib.Data.Variant.append _ok <<< f) v

_notFound = SProxy ∷ SProxy "notFound"

type NotFound a res
  = ( notFound ∷ a | res )

notFound ∷ ∀ a b res. Row.Lacks "ok" res ⇒ a → Okayish (NotFound a + res) b
notFound = notOk <<< Variant.inj _notFound

_found = SProxy ∷ SProxy "found"

type Found res
  = ( found ∷ String | res )

found ∷ ∀ a res. Row.Lacks "ok" res ⇒ String → Okayish (Found + res) a
found = notOk <<< Variant.inj _found

_badRequest = SProxy ∷ SProxy "badRequest"

type BadRequest a res
  = ( badRequest ∷ a | res )

badRequest ∷ ∀ a b res. Row.Lacks "ok" res ⇒ a → Okayish (BadRequest a + res) b
badRequest = notOk <<< Variant.inj _badRequest
