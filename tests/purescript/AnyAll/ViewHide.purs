module Test.AnyAll.ViewHideTest where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.HashMap as Map
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import AnyAll.Relevance (relevant)
import AnyAll.Types

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
      , mark: (Default (Left Nothing))
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
      let qq = relevant Hard DPNormal (left true) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just F / Parent Just T" do
      let qq = relevant Hard DPNormal (left false) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Just T" do
      let qq = relevant Hard DPNormal (right true) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just F / Parent Just T" do
      let qq = relevant Hard DPNormal (right false) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Nothing / Parent Just T" do
      let qq = relevant Hard DPNormal emptyMarking (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Just F" do
      let qq = relevant Hard DPNormal (left true) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just F / Parent Just F" do
      let qq = relevant Hard DPNormal (left false) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Just F" do
      let qq = relevant Hard DPNormal (right true) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just F / Parent Just F" do
      let qq = relevant Hard DPNormal (right false) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Nothing / Parent Just F" do
      let qq = relevant Hard DPNormal emptyMarking (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Nothing" do
      let qq = relevant Hard DPNormal (left true) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` Ask
    it "Self Just F / Parent Nothing" do
      let qq = relevant Hard DPNormal (left false) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` Ask
    it "Self Just T / Parent Nothing" do
      let qq = relevant Hard DPNormal (right true) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just F / Parent Nothing" do
      let qq = relevant Hard DPNormal (right false) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Nothing / Parent Nothing" do
      let qq = relevant Hard DPNormal emptyMarking Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` Ask
  describe "Soft" do
    it "Self Just T / Parent Just T" do
      let qq = relevant Soft DPNormal (left true) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just F / Parent Just T" do
      let qq = relevant Soft DPNormal (left false) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Just T" do
      let qq = relevant Soft DPNormal (right true) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just F / Parent Just T" do
      let qq = relevant Soft DPNormal (right false) (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Nothing / Parent Just T" do
      let qq = relevant Soft DPNormal emptyMarking (Just true) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Just F" do
      let qq = relevant Soft DPNormal (left true) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just F / Parent Just F" do
      let qq = relevant Soft DPNormal (left false) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just T / Parent Just F" do
      let qq = relevant Soft DPNormal (right true) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just F / Parent Just F" do
      let qq = relevant Soft DPNormal (right false) (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Nothing / Parent Just F" do
      let qq = relevant Soft DPNormal emptyMarking (Just false) example1_nl keyLeaf
      getShouldView qq `shouldEqual` Hide
    it "Self Just T / Parent Nothing" do
      let qq = relevant Soft DPNormal (left true) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` Ask
    it "Self Just F / Parent Nothing" do
      let qq = relevant Soft DPNormal (left false) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` Ask
    it "Self Just T / Parent Nothing" do
      let qq = relevant Soft DPNormal (right true) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Just F / Parent Nothing" do
      let qq = relevant Soft DPNormal (right false) Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` View
    it "Self Nothing / Parent Nothing" do
      let qq = relevant Soft DPNormal emptyMarking Nothing example1_nl keyLeaf
      getShouldView qq `shouldEqual` Ask
