module Data.Foreign.Callback where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, F, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Function.Uncurried (Fn1, Fn2, mkFn1, mkFn2, runFn1, runFn2)
import Partial.Unsafe (unsafeCrashWith)

newtype CB1 a b = CB1 (Fn1 a b)
newtype CB2 a b c = CB2 (Fn2 a b c)

wrap1 :: forall a b. (a -> b) -> CB1 a b
wrap1 = CB1 <<< mkFn1

wrap2 :: forall a b c. (a -> b -> c) -> CB2 a b c
wrap2 = CB2 <<< mkFn2

encodeRes :: forall a. Encode a => F a -> Foreign
encodeRes = encode <<< either (unsafeCrashWith <<< show) id <<< runExcept

instance encodeCB1 :: (Decode a, Encode b) => Encode (CB1 a b) where
  encode (CB1 f) = toForeign $ mkFn1 \a -> encodeRes $ runFn1 f <$> decode a

instance encodeCB2 :: (Decode a, Decode b, Encode c) => Encode (CB2 a b c) where
  encode (CB2 f) = toForeign $ mkFn2 \a b -> encodeRes $ runFn2 f <$> decode a <*> decode b

