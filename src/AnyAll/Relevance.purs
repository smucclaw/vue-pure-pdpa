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
relevant :: Marking -> Maybe Boolean -> NLDict -> Item String -> Q
relevant marking parentValue nl self =
  let
    selfValue = evaluate marking self
    initVis =
      if isJust parentValue then
        if parentValue == selfValue then View
        else Hide
      else if isJust (evaluate marking self) then View
      else Ask
    -- we compute the initial visibility of the subtree.
    -- if our initial visibility is to hide, then we mute all our children by converting Ask to Hide; but if any of our children are View, we leave them as View.
    paintedChildren = (if initVis /= Hide then identity else ask2hide) <$> relevant marking selfValue nl <$> getChildren self
    makeQNode itemNode = case itemNode of
      Leaf x -> mkQ (initVis) (Simply x) (nlMapFn x nl nl) Nothing (lookupMarking x marking) []
      Not x -> makeQNode x -- [TODO] we should have a SimplyNot as well
      Any label _ -> mkQ (ask2view initVis) Or  (nlMapFn (label2pre label) nl nl) (Just label) (Default $ Left selfValue) paintedChildren
      All label _ -> mkQ (ask2view initVis) And (nlMapFn (label2pre label) nl nl) (Just label) (Default $ Left selfValue) paintedChildren
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

relevantQ :: Marking -> Maybe Boolean -> NLDict -> Item String -> Q
relevantQ marking parentValue nl self =
  let
    selfValue = evaluateHard marking self
    initVis =
      if isJust parentValue then
        if parentValue == selfValue then View
        else Hide
      else if isJust (evaluateHard marking self) then View
      else Ask
    -- we compute the initial visibility of the subtree.
    -- if our initial visibility is to hide, then we mute all our children by converting Ask to Hide; but if any of our children are View, we leave them as View.
    paintedChildren = (if initVis /= Hide then identity else ask2hide) <$> relevantQ marking selfValue nl <$> getChildren self
    makeQNode itemNode = case itemNode of
      Leaf x -> mkQ (initVis) (Simply x) (nlMapFn x nl nl) Nothing (lookupMarking x marking) []
      Not x -> makeQNode x -- [TODO] we should have a SimplyNot as well
      Any label _ -> mkQ (ask2view initVis) Or  (nlMapFn (label2pre label) nl nl) (Just label) (Default $ Left selfValue) paintedChildren
      All label _ -> mkQ (ask2view initVis) And (nlMapFn (label2pre label) nl nl) (Just label) (Default $ Left selfValue) paintedChildren
  in -- convert to a QTree for output
    makeQNode self

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
evaluate :: Marking -> Item String -> Maybe Boolean
evaluate (Marking marking) (Leaf x) = case Map.lookup x marking of
  Just (Default (Right (Just y))) -> Just y
  _ -> Nothing

evaluate  marking (Not item) = not <$> (evaluate marking item)
evaluate  marking (Any _ items) = evaluateAny (evaluate marking <$> items)
evaluate  marking (All _ items) = evaluateAll (evaluate marking <$> items)

-- well, it depends on what values the children have. and that depends on whether we're assessing them in soft or hard mode.
evaluateHard :: Marking -> Item String -> Maybe Boolean
evaluateHard (Marking marking) (Leaf x) = case Map.lookup x marking of
  Just (Default (Right (Just y))) -> Just y
  _ -> Nothing

evaluateHard marking (Not item) = not <$> (evaluateHard marking item)
evaluateHard marking (Any _ items) = evaluateAny (evaluateHard marking <$> items)
evaluateHard marking (All _ items) = evaluateAll (evaluateHard marking <$> items)

evaluateAny :: forall f. Foldable f => f (Maybe Boolean) -> Maybe Boolean
evaluateAny items
  | Just true `elem` items = Just true
  | all (_ == Just false) items = Just false
  | otherwise = Nothing

evaluateAll :: forall f. Foldable f => f (Maybe Boolean) -> Maybe Boolean
evaluateAll items
  | all (_ == Just true) items = Just true
  | Just false `elem` items = Just false
  | otherwise = Nothing

lookupMarking :: String -> Marking -> Default Boolean
lookupMarking node marking = fromMaybe (Default $ Left Nothing) (Map.lookup node (getMarking marking))
