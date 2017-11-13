module Data.Foreign.Unsafe.Function where

import Prelude

import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Foreign.Unsafe.Helpers (unsafeDecode)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn2, mkFn3, runFn2, runFn3)
import Data.Newtype (class Newtype, un)


newtype Func1 a b = Func1 (Fn1 a b)
newtype Func2 a b c = Func2 (Fn2 a b c)
newtype Func3 a b c d = Func3 (Fn3 a b c d)
derive instance newtypeFunc1 :: Newtype (Func1 a b) _
derive instance newtypeFunc2 :: Newtype (Func2 a b c) _
derive instance newtypeFunc3 :: Newtype (Func3 a b c d) _

decodeFunc1 :: forall a b. Encode a => Decode b => Foreign -> Func1 a b
decodeFunc1 f = Func1 $ \a -> unsafeDecode $ (unsafeFromForeign f) (encode a)

decodeFunc2 :: forall a b c. Encode a => Encode b => Decode c => Foreign -> Func2 a b c
decodeFunc2 f = Func2 $ mkFn2 \a b -> unsafeDecode $ runFn2 (unsafeFromForeign f) (encode a) (encode b)

decodeFunc3 :: forall a b c d. Encode a => Encode b => Encode c => Decode d => Foreign -> Func3 a b c d
decodeFunc3 f = Func3 $ mkFn3 \a b c -> unsafeDecode $ runFn3 (unsafeFromForeign f) (encode a) (encode b) (encode c)

mkFunc1 :: forall a b. Encode a => Decode b => Foreign -> (a -> b)
mkFunc1 f = un Func1 (decodeFunc1 f)

mkFunc2 :: forall a b c. Encode a => Encode b => Decode c => Foreign -> (a -> b -> c)
mkFunc2 f = runFn2 $ un Func2 (decodeFunc2 f)

mkFunc3 :: forall a b c d. Encode a => Encode b => Encode c => Decode d => Foreign -> (a -> b -> c -> d)
mkFunc3 f = runFn3 $ un Func3 (decodeFunc3 f)

