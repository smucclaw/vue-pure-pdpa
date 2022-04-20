module Test.AnyAll.RelevanceTest where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import AnyAll.Types (Marking(..), Item(..), Hardness(..), Default(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import AnyAll.Relevance (evaluate)
import AnyAll.Types (Label(..))
import Data.List.Types (List)

keyString :: String
keyString = "key"

right :: Boolean -> Marking
right b = Marking $ Default <$> Map.fromFoldable [ Tuple keyString $ Right (Just b) ]

left :: Boolean -> Marking
left b = Marking $ Default <$> Map.fromFoldable [ Tuple keyString $ Left (Just b) ]

keyLeaf :: Item String
keyLeaf = Leaf keyString

missingLeaf :: Item String
missingLeaf = Leaf "missing"

any :: Array String -> Item String
any leafs = (Any (Pre "dummy") (Leaf <$> leafs))

all :: Array String -> Item String
all leafs = (All (Pre "dummy") (Leaf <$> leafs))

spec :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
spec = describe "evaluate" do
  describe "leaf" do
    describe "key present in marking" do
      it "Soft matching right true" do
        evaluate Soft (right true) keyLeaf `shouldEqual` (Just true)
      it "Soft matching right false" do
        evaluate Soft (right false) keyLeaf `shouldEqual` (Just false)
      it "Soft matching left true" do
        evaluate Soft (left true) keyLeaf `shouldEqual` (Just true)
      it "Soft matching left false" do
        evaluate Soft (left false) keyLeaf `shouldEqual` (Just false)
      it "Hard matching right true" do
        evaluate Hard (right true) keyLeaf `shouldEqual` (Just true)
      it "Hard matching right false" do
        evaluate Hard (right false) keyLeaf `shouldEqual` (Just false)
      it "Hard matching left Nothing" do
        evaluate Hard (left true) keyLeaf `shouldEqual` Nothing
      it "Hard matching left Nothing" do
        evaluate Hard (left true) keyLeaf `shouldEqual` Nothing

    describe "key is not present in marking" do
      it "Soft missing right true" do
        evaluate Soft (right true) missingLeaf `shouldEqual` Nothing
      it "Soft missing right false" do
        evaluate Soft (right false) missingLeaf `shouldEqual` Nothing
      it "Soft missing left true" do
        evaluate Soft (left true) missingLeaf `shouldEqual` Nothing
      it "Soft missing left false" do
        evaluate Soft (left false) missingLeaf `shouldEqual` Nothing
      it "Hard missing right true" do
        evaluate Hard (right true) missingLeaf `shouldEqual` Nothing
      it "Hard missing right false" do
        evaluate Hard (right false) missingLeaf `shouldEqual` Nothing
      it "Hard missing left Nothing" do
        evaluate Hard (left true) missingLeaf `shouldEqual` Nothing
      it "Hard missing left Nothing" do
        evaluate Hard (left true) missingLeaf `shouldEqual` Nothing

  describe "Any" do
    it "true present" do
      evaluate Soft (right true) (any [ "key", "run" ]) `shouldEqual` (Just true)
    it "all false" do
      evaluate Soft (right false) (any [ "key" ]) `shouldEqual` (Just false)
    it "missing key" do
      evaluate Soft (right false) (any [ "missing" ]) `shouldEqual` Nothing

  describe "All" do
    it "all true" do
      evaluate Soft (right true) (all [ "key" ]) `shouldEqual` (Just true)
    it "false present" do
      evaluate Soft (right false) (all [ "key", "run" ]) `shouldEqual` (Just false)
    it "missing key" do
      evaluate Soft (right false) (all [ "missing" ]) `shouldEqual` Nothing
