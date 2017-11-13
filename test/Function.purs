module Test.Function where

import Prelude

import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Function (mkFunc2)
import Jack (Property, forAll, property)
import Test.Fruit (Fruit, fruitIdSafe, genFruit)
import Test.JS (v2Impl)

v2Safe :: Fruit -> Fruit -> Either MultipleErrors (Array Fruit)
v2Safe = mkFunc2 v2Impl

prop_can_call_js :: Property
prop_can_call_js =
  forAll genFruit \fruit ->
  property $ fruitIdSafe fruit == Right fruit

prop_can_call_js2 :: Property
prop_can_call_js2 =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->
  property $ v2Safe fruit fruit2 == Right [fruit, fruit2]
