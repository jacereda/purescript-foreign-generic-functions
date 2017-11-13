module Data.Foreign.Function where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foreign (F, Foreign, MultipleErrors, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Function.Uncurried (Fn1, Fn2, mkFn2, runFn2)
import Data.Newtype (class Newtype, un)

newtype Func1 a b = Func1 (Fn1 a (F b))
newtype Func2 a b c = Func2 (Fn2 a b (F c))

derive instance newtypeFunc1 :: Newtype (Func1 a b) _
derive instance newtypeFunc2 :: Newtype (Func2 a b c) _

decodeFunc1 :: forall a b. Encode a => Decode b => Foreign -> Func1 a b
decodeFunc1 f = Func1 $ \a -> decode $ (unsafeFromForeign f) (encode a)

decodeFunc2 :: forall a b c. Encode a => Encode b => Decode c => Foreign -> Func2 a b c
decodeFunc2 f = Func2 $ mkFn2 \a b -> decode $ runFn2 (unsafeFromForeign f) (encode a) (encode b)

wrap1 :: forall a b. Encode a => Decode b => Foreign -> (a -> Either MultipleErrors b)
wrap1 f = \a -> runExcept (un Func1 (decodeFunc1 f) a)

wrap2 :: forall a b c. Encode a => Encode b => Decode c => Foreign -> (a -> b -> Either MultipleErrors c)
wrap2 f = \a b -> runExcept (runFn2 (un Func2 (decodeFunc2 f)) a b)

instance func1Decode :: (Encode a, Decode b) => Decode (Func1 a b) where
  decode f = pure (decodeFunc1 f)

instance func2Decode :: (Encode a, Encode b, Decode c) => Decode (Func2 a b c) where
  decode f = pure (decodeFunc2 f)

