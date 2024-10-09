module Test.AnyAll.RelevanceTest
  ( all
  , any
  , keyLeaf
  , keyString
  , missingLeaf
  , not
  , right
  , spec
  )
  where

import Prelude

import AnyAll.Relevance (evaluate)
import AnyAll.Types (Item(..), Label(..), Marking(..), Ternary(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

keyString :: String
keyString = "key"

right :: Ternary -> Marking
right b = Marking $ Map.fromFoldable [ Tuple keyString  b]

keyLeaf :: Item String
keyLeaf = Leaf keyString

missingLeaf :: Item String
missingLeaf = Leaf "missing"

any :: Array String -> Item String
any leafs = (Any (Pre "dummy") (Leaf <$> leafs))

all :: Array String -> Item String
all leafs = (All (Pre "dummy") (Leaf <$> leafs))

not :: String -> Item String
not leaf = (Not (Leaf leaf))

spec :: Spec Unit
spec = describe "evaluate" do
  describe "leaf" do
    describe "key present in marking" do
      it "Hard matching right true" do
        evaluate  (right True) keyLeaf `shouldEqual` True
      it "Hard matching right false" do
        evaluate  (right False) keyLeaf `shouldEqual` False

    describe "key is not present in marking" do
      it "Hard missing right true" do
        evaluate  (right True) missingLeaf `shouldEqual` Unknown
      it "Hard missing right false" do
        evaluate  (right False) missingLeaf `shouldEqual` Unknown

  describe "Any" do
    it "true present" do
      evaluate  (right True) (any [ "key", "run" ]) `shouldEqual` True
    it "all false" do
      evaluate  (right False) (any [ "key" ]) `shouldEqual` False
    it "missing key" do
      evaluate  (right False) (any [ "missing" ]) `shouldEqual` Unknown

  describe "All" do
    it "all true" do
      evaluate  (right True) (all [ "key" ]) `shouldEqual` True
    it "false present" do
      evaluate  (right False) (all [ "key", "run" ]) `shouldEqual` False
    it "missing key" do
      evaluate  (right False) (all [ "missing" ]) `shouldEqual` Unknown

  describe "Not" do
    it "not true" do
      evaluate  (right True) (not "key") `shouldEqual` False
    it "not false" do
      evaluate  (right False) (not "key") `shouldEqual` True
    it "missing key" do
      evaluate  (right False) (not "missing") `shouldEqual` Unknown
