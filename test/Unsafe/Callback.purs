module Test.Unsafe.Callback where

import Prelude

import Data.Foreign.Unsafe.Callback (CB1, CB2, mkCB1, mkCB2)
import Data.Foreign.Unsafe.Function (mkFunc2, mkFunc3)
import Jack (Property, forAll, property)
import Test.Fruit (Fruit, fruitPair, genFruit)
import Test.JS (testCB1Impl, testCB2Impl)
import Test.Unsafe.Function (fruitIdUnsafe)

testCB1Unsafe :: CB1 Fruit Fruit -> Fruit -> Fruit
testCB1Unsafe = mkFunc2 testCB1Impl

testCB2Unsafe :: CB2 Fruit Fruit (Array Fruit) -> Fruit -> Fruit -> Array Fruit
testCB2Unsafe = mkFunc3 testCB2Impl

prop_cb1_works_unsafe :: Property
prop_cb1_works_unsafe =
  forAll genFruit \fruit ->
  property $ testCB1Unsafe (mkCB1 fruitIdUnsafe) fruit == fruit


prop_cb2_works_unsafe :: Property
prop_cb2_works_unsafe =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->  
  property $ testCB2Unsafe (mkCB2 fruitPair) fruit fruit2 == [fruit, fruit2]

