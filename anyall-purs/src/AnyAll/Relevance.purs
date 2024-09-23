module AnyAll.Relevance where

import AnyAll.Types
import Prelude

import Data.Set as Set
import Data.Tuple
import Data.Map as Map
import Data.List (any, all, elem, List(..))
import Data.Foldable (class Foldable)

import Data.Maybe
import Data.Either (Either(..), either)

-- paint a tree as View, Hide, or Ask, depending on the dispositivity of the current node and its children.
relevant :: Marking -> Ternary -> Item String -> Q
relevant marking parentValue self =
  let
    selfValue = evaluate marking self
    initVis =
      if parentValue /= Unknown then
        if parentValue == selfValue then View
        else Hide
      else if (evaluate marking self) /= Unknown then View
      else Ask
    -- we compute the initial visibility of the subtree.
    -- if our initial visibility is to hide, then we mute all our children by converting Ask to Hide; but if any of our children are View, we leave them as View.
    paintedChildren = (if initVis /= Hide then identity else ask2hide) <$> relevant marking selfValue <$> getChildren self
    makeQNode itemNode = case itemNode of
      Leaf x -> mkQ (initVis) (Simply x) Nothing (lookupMarking x marking) []
      Not x -> makeQNode x -- [TODO] we should have a SimplyNot as well
      Any label _ -> mkQ (ask2view initVis) Or  (Just label) selfValue paintedChildren
      All label _ -> mkQ (ask2view initVis) And (Just label) selfValue paintedChildren
  in -- convert to a QTree for output
    makeQNode self

getChildren (Leaf _) = []
getChildren (Not x) = getChildren x
getChildren (Any _ c) = c
getChildren (All _ c) = c

ask2hide :: Q -> Q
ask2hide (Q q@{ shouldView: Ask }) = Q $ q { shouldView = Hide }
ask2hide x = x

ask2view :: ShouldView -> ShouldView
ask2view Ask = View
ask2view x = x

nlMapFn ∷ String → Map.Map String (Map.Map String String) → Map.Map String (Map.Map String String) → Map.Map String String
nlMapFn word nldict nl =
  let
    langs = Set.toUnfoldable $ Map.keys nldict :: Array String
  in
    Map.fromFoldable $ do
      lg <- langs
      let
        lgDict = fromMaybe Map.empty (Map.lookup lg nl)
        longtext = fromMaybe "" (Map.lookup word lgDict)
      pure $ Tuple lg longtext

-- well, it depends on what values the children have. and that depends on whether we're assessing them in soft or hard mode.
evaluate :: Marking -> Item String -> Ternary
evaluate (Marking marking) (Leaf x) = case Map.lookup x marking of
  Just y -> y
  _ -> Unknown

evaluate  marking (Not item) = not3 (evaluate marking item)
evaluate  marking (Any _ items) = evaluateAny (evaluate marking <$> items)
evaluate  marking (All _ items) = evaluateAll (evaluate marking <$> items)

evaluateAny :: forall f. Foldable f => f Ternary -> Ternary
evaluateAny items
  | True `elem` items = True
  | all (_ == False) items = False
  | otherwise = Unknown

evaluateAll :: forall f. Foldable f => f Ternary -> Ternary
evaluateAll items
  | all (_ == True) items = True
  | False `elem` items = False
  | otherwise = Unknown

lookupMarking :: String -> Marking -> Ternary
lookupMarking node marking = fromMaybe Unknown (Map.lookup node (getMarking marking))
