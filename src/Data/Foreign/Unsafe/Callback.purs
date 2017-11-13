module Data.Foreign.Unsafe.Callback where

import Prelude

import Data.Foreign (toForeign)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Foreign.Unsafe.Helpers (unsafeDecode)
import Data.Function.Uncurried (Fn1, Fn2, mkFn2, runFn2)

newtype CB1 a b = CB1 (Fn1 a b)
newtype CB2 a b c = CB2 (Fn2 a b c)

mkCB1 :: forall a b. (a -> b) -> CB1 a b
mkCB1 = CB1

mkCB2 :: forall a b c. (a -> b -> c) -> CB2 a b c
mkCB2 = CB2 <<< mkFn2

instance encodeCB1 :: (Decode a, Encode b) => Encode (CB1 a b) where
  encode (CB1 f) = toForeign \a -> encode $ f $ unsafeDecode a

instance encodeCB2 :: (Decode a, Decode b, Encode c) => Encode (CB2 a b c) where
  encode (CB2 f) = toForeign $ mkFn2 \a b -> encode $ runFn2 f (unsafeDecode a) (unsafeDecode b)


