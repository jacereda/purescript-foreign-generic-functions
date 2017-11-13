module Test.Unsafe.Function where

import Data.Foreign.Unsafe.Function (wrap1, wrap2)
import Jack (Property, forAll, (===))
import Test.Fruit (Fruit, genFruit)
import Test.JS (fruitIdImpl, v2Impl)

fruitIdUnsafe :: Fruit -> Fruit
fruitIdUnsafe = wrap1 fruitIdImpl

v2Unsafe :: Fruit -> Fruit -> Array Fruit
v2Unsafe = wrap2 v2Impl

prop_can_call_js :: Property
prop_can_call_js =
  forAll genFruit \fruit ->
  fruitIdUnsafe fruit === fruit

prop_can_call_js2 :: Property
prop_can_call_js2 =
  forAll genFruit \fruit ->
  forAll genFruit \fruit2 ->
  v2Unsafe fruit fruit2 === [fruit, fruit2]

