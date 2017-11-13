module Test.Fruit where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Jack (Gen, elements)

data Fruit = Apple
           | Banana
           | Orange

derive instance genericFruit :: Generic Fruit _
instance showFruit :: Show Fruit where show = genericShow
instance eqFruit :: Eq Fruit where eq = genericEq
instance encodeFruit :: Encode Fruit where encode = genericEncode defaultOptions
instance decodeFruit :: Decode Fruit where decode = genericDecode defaultOptions

genFruit :: Gen Fruit
genFruit = elements [ Apple, Banana, Orange ]

fruitPair :: Fruit -> Fruit -> Array Fruit
fruitPair a b = [a, b]
