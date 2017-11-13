module Test.Unsafe.Function where

import Prelude

import Data.Either (Either(..))
import Data.Foreign.Unsafe.Function (mkFunc2)
import Jack (Property, forAll, property)
import Test.Fruit (Fruit, fruitIdSafe, fruitIdUnsafe, genFruit)
import Test.JS (v2Impl)

v2Unsafe :: Fruit -> Fruit -> Array Fruit
v2Unsafe = mkFunc2 v2Impl

prop_can_call_js :: Property
prop_can_call_js =
  forAll genFruit \fruit ->
  property $ fruitIdSafe fruit == Right fruit

prop_can_call_js_unsafe :: Property
prop_can_call_js_unsafe =
  forAll genFruit \fruit ->
  property $ fruitIdUnsafe fruit == fruit

prop_can_call_js_unsafe2 :: Property
prop_can_call_js_unsafe2 =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->
  property $ v2Unsafe fruit fruit2 == [fruit, fruit2]

