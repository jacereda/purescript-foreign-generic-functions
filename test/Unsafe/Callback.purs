module Test.Unsafe.Callback where

import Prelude

import Data.Foreign.Unsafe.Callback as UnsafeCB
import Data.Foreign.Unsafe.Function as Unsafe
import Jack (Property, forAll, property)
import Test.Fruit (Fruit, fruitIdUnsafe, fruitPair, genFruit)
import Test.JS (testCB1Impl, testCB2Impl)


testCB1Unsafe :: UnsafeCB.CB1 Fruit Fruit -> Fruit -> Fruit
testCB1Unsafe = Unsafe.mkFunc2 testCB1Impl


testCB2Unsafe :: UnsafeCB.CB2 Fruit Fruit (Array Fruit) -> Fruit -> Fruit -> Array Fruit
testCB2Unsafe = Unsafe.mkFunc3 testCB2Impl


prop_cb1_works_unsafe :: Property
prop_cb1_works_unsafe =
  forAll genFruit \fruit ->
  property $ testCB1Unsafe (UnsafeCB.mkCB1 fruitIdUnsafe) fruit == fruit


prop_cb2_works_unsafe :: Property
prop_cb2_works_unsafe =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->  
  property $ testCB2Unsafe (UnsafeCB.mkCB2 fruitPair) fruit fruit2 == [fruit, fruit2]

