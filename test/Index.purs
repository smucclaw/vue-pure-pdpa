module Test.IndexTest where

import Index (str)

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
spec = describe "Index" do
  it "str" do
    str `shouldEqual` "hello world from purescript!"
