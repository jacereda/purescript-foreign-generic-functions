module Test.Function where

import Prelude

import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Function (mkFunc1, mkFunc2)
import Jack (Property, forAll, property)
import Test.Fruit (Fruit, genFruit)
import Test.JS (fruitIdImpl, v2Impl)

fruitIdSafe :: Fruit -> Either MultipleErrors Fruit
fruitIdSafe = mkFunc1 fruitIdImpl

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
