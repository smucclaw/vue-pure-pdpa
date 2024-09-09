module Test.AnyAll.ViewHideTest where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import AnyAll.Relevance (relevant)
import AnyAll.Types

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

emptyMarking :: Marking
emptyMarking = markup Map.empty

example1_nl :: NLDict
example1_nl = Map.empty

example1 :: Item String
example1 =
  (Leaf "single")

myq :: Q
myq =
  ( Q
      { andOr: (Simply "single")
      , children: []
      , mark: Unknown
      , prePost: Nothing
      , shouldView: Ask
      , tagNL: Map.empty
      }
  )

getShouldView :: Q -> ShouldView
getShouldView (Q { shouldView: sh }) = sh

derive1 :: ShouldView -> ShouldView
derive1 initVis = if initVis /= Hide then initVis else Hide

spec :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
spec = describe "hide view" do
  describe "Hard" do
    it "Self Just T / Parent Just T" do
      let qq = relevant (right True) True example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just F / Parent Just T" do
      let qq = relevant (right False) True example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Nothing / Parent Just T" do
      let qq = relevant emptyMarking True example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Just F" do
      let qq = relevant (right True) False example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just F / Parent Just F" do
      let qq = relevant (right False) False example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Nothing / Parent Just F" do
      let qq = relevant emptyMarking False example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Nothing" do
      let qq = relevant (right True) Unknown example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just F / Parent Nothing" do
      let qq = relevant (right False) Unknown example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Nothing / Parent Nothing" do
      let qq = relevant emptyMarking Unknown example1_nl keyLeaf
      getShouldView qq `shouldEqual` Ask
