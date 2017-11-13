module Test.Unsafe.Callback where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Unsafe.Callback (CB1, CB2, mkCB1, mkCB2)
import Data.Foreign.Unsafe.Function (mkFunc2, mkFunc3)
import Jack (Property, forAll, (===))
import Test.Fruit (Fruit, fruitPair, genFruit)
import Test.JS (testCB1Impl, testCB2Impl)
import Test.Unsafe.Function (fruitIdUnsafe)

testCB1Unsafe :: forall a b. Encode a => Decode a => Encode b => Decode b => CB1 a b -> a -> b
testCB1Unsafe = mkFunc2 testCB1Impl

testCB2Unsafe :: CB2 Fruit Fruit (Array Fruit) -> Fruit -> Fruit -> Array Fruit
testCB2Unsafe = mkFunc3 testCB2Impl

effCB1 :: forall e. Fruit -> Eff (console :: CONSOLE | e) Fruit
effCB1 f = do
  logShow f
  pure f

prop_cb1_works :: Property
prop_cb1_works =
  forAll genFruit \fruit ->
  testCB1Unsafe (mkCB1 fruitIdUnsafe) fruit === fruit

prop_cb2_works :: Property
prop_cb2_works =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->  
  testCB2Unsafe (mkCB2 fruitPair) fruit fruit2 === [fruit, fruit2]
