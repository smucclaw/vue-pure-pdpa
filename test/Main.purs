module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.AnyAll.RelevanceTest (spec) as RelevanceTest

main :: Effect Unit
main = do
  log "🍝"
  launchAff_ $ runSpec [consoleReporter] do
    RelevanceTest.spec

-- https://purescript-spec.github.io/purescript-spec/
