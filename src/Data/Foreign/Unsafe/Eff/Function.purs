module Data.Foreign.Unsafe.Eff.Function where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn1, mkEffFn2, mkEffFn3, runEffFn1, runEffFn2, runEffFn3)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Foreign.Unsafe.Helpers (unsafeDecode)
import Data.Newtype (class Newtype, un)


newtype Func1 eff a b = Func1 (EffFn1 eff a b)
newtype Func2 eff a b c = Func2 (EffFn2 eff a b c)
newtype Func3 eff a b c d = Func3 (EffFn3 eff a b c d)
derive instance newtypeFunc1 :: Newtype (Func1 eff a b) _
derive instance newtypeFunc2 :: Newtype (Func2 eff a b c) _
derive instance newtypeFunc3 :: Newtype (Func3 eff a b c d) _

decodeFunc1 :: forall eff a b. Encode a => Decode b => Foreign -> Func1 eff a b
decodeFunc1 f = Func1 $ mkEffFn1 \a -> pure <<< unsafeDecode =<< runEffFn1 (unsafeFromForeign f) (encode a)

decodeFunc2 :: forall eff a b c. Encode a => Encode b => Decode c => Foreign -> Func2 eff a b c
decodeFunc2 f = Func2 $ mkEffFn2 \a b -> pure <<< unsafeDecode =<< runEffFn2 (unsafeFromForeign f) (encode a) (encode b)

decodeFunc3 :: forall eff a b c d. Encode a => Encode b => Encode c => Decode d => Foreign -> Func3 eff a b c d
decodeFunc3 f = Func3 $ mkEffFn3 \a b c -> pure <<< unsafeDecode =<< runEffFn3 (unsafeFromForeign f) (encode a) (encode b) (encode c)

wrap1 :: forall eff a b. Encode a => Decode b => Foreign -> (a -> Eff eff b)
wrap1 f = runEffFn1 $ un Func1 (decodeFunc1 f)

wrap2 :: forall eff a b c. Encode a => Encode b => Decode c => Foreign -> (a -> b -> Eff eff c)
wrap2 f = runEffFn2 $ un Func2 (decodeFunc2 f)

wrap3 :: forall eff a b c d. Encode a => Encode b => Encode c => Decode d => Foreign -> (a -> b -> c -> Eff eff d)
wrap3 f = runEffFn3 $ un Func3 (decodeFunc3 f)

