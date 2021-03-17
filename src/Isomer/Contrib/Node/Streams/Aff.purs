module Isomer.Contrib.Node.Streams.Aff where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.Encoding (Encoding)
import Node.Stream (Stream, Writable)
import Node.Stream (writeString) as Node.Streams


-- | TODO: Are we able to handle somehow the write result?
-- | "false if the stream wishes for the calling code to wait for the 'drain' event
-- | to be emitted before continuing to write additional data; otherwise true."
writeString ∷ ∀ m r.  MonadAff m ⇒ Writable r → Encoding → String → m Unit
writeString stream encoding str = do
  liftAff $ makeAff go
  where
    go handler = do
      void $ Node.Streams.writeString stream encoding str (handler (Right unit))
      pure nonCanceler
