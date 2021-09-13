module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Index (spec) as Index


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Index.spec

-- https://purescript-spec.github.io/purescript-spec/#full-example