module Test.IndexTest where

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

import Index (str)

spec = describe "index.spec" do
  it "str" do
     str `shouldEqual` "hello world from purescript!" 
