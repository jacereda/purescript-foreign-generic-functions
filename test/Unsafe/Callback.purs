module Test.Unsafe.Callback where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Unsafe.Callback (CB1, CB2, wrap1, wrap2)
import Data.Foreign.Unsafe.Function as Function
import Jack (Property, forAll, (===))
import Test.Fruit (Fruit, fruitPair, genFruit)
import Test.JS (testCB1Impl, testCB2Impl)
import Test.Unsafe.Function (fruitIdUnsafe)

testCB1Unsafe :: forall a b. Encode a => Decode a => Encode b => Decode b => CB1 a b -> a -> b
testCB1Unsafe = Function.wrap2 testCB1Impl

testCB2Unsafe :: CB2 Fruit Fruit (Array Fruit) -> Fruit -> Fruit -> Array Fruit
testCB2Unsafe = Function.wrap3 testCB2Impl

prop_cb1_works :: Property
prop_cb1_works =
  forAll genFruit \fruit ->
  testCB1Unsafe (wrap1 fruitIdUnsafe) fruit === fruit

prop_cb2_works :: Property
prop_cb2_works =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->  
  testCB2Unsafe (wrap2 fruitPair) fruit fruit2 === [fruit, fruit2]
