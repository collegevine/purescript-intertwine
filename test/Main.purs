module Test.Main where

import Prelude

import Effect (Effect)
import Test.Text as Text
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Text.allTests
