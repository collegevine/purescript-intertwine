module Test.Main where

import Prelude

import Effect (Effect)
import Test.Combinators as Combinators
import Test.Route as Route
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.Text as Text

main :: Effect Unit
main = run [consoleReporter] do
    Text.allTests
    Route.allTests
    Combinators.allTests
