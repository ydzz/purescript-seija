module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (error, log)

import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  error $ unsafeCoerce $ 1 /\ 1
  log "You should add some tests."
