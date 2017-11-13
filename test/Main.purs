module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Jack (jackMain)

main :: forall e. Eff (random :: RANDOM, console :: CONSOLE | e) Unit
main = jackMain [ "Test.Function"
                , "Test.Callback"
                , "Test.Unsafe.Function"
                , "Test.Unsafe.Callback"
                , "Test.Unsafe.Eff.Function"
                ]
