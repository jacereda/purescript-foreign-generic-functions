module Data.Foreign.Unsafe.Helpers where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, decode)
import Partial.Unsafe (unsafeCrashWith)

unsafeDecode :: forall a. Decode a => Foreign -> a
unsafeDecode a = case runExcept $ decode a of
  Right x -> x
  Left e -> unsafeCrashWith $ show e
