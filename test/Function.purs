module Test.Function where

import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Function (wrap1, wrap2)
import Jack (Property, forAll, (===))
import Test.Fruit (Fruit, genFruit)
import Test.JS (fruitIdImpl, v2Impl)

fruitIdSafe :: Fruit -> Either MultipleErrors Fruit
fruitIdSafe = wrap1 fruitIdImpl

v2Safe :: Fruit -> Fruit -> Either MultipleErrors (Array Fruit)
v2Safe = wrap2 v2Impl

prop_can_call_js :: Property
prop_can_call_js =
  forAll genFruit \fruit ->
  fruitIdSafe fruit === Right fruit

prop_can_call_js2 :: Property
prop_can_call_js2 =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->
  v2Safe fruit fruit2 === Right [fruit, fruit2]
