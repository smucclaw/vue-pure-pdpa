module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.AnyAll.RelevanceTest (spec) as RelevanceTest
import Test.AnyAll.RenderingTest (spec) as RenderingTest
import Test.AnyAll.ViewHideTest (spec) as ViewHideTest

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] do
    RelevanceTest.spec
    RenderingTest.spec
    ViewHideTest.spec

-- https://purescript-spec.github.io/purescript-spec/
