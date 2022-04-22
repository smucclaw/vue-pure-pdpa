module AnyAll.Relevance where

import AnyAll.Types
import Prelude

import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

import Data.Set as Set
import Data.Tuple
import Data.Map as Map
import Data.List (any, all, elem, List(..))
import Data.Foldable (class Foldable)

import Data.Maybe
import Data.Either (Either(..), either)

-- paint a tree as View, Hide, or Ask, depending on the dispositivity of the current node and its children.
relevant :: Hardness -> DisplayPref -> Marking -> Maybe Bool -> NLDict -> Item String -> Q
relevant sh dp marking parentValue nl self =
  let
    selfValue = evaluate sh marking self
    initVis =
      if isJust parentValue then
        if parentValue == selfValue then View
        else Hide
      else if isJust (evaluate Hard marking self) then View
      else Ask
    -- we compute the initial visibility of the subtree.
    -- if our initial visibility is to hide, then we mute all our children by converting Ask to Hide; but if any of our children are View, we leave them as View.
    paintedChildren = (if initVis /= Hide then identity else ask2hide) <$> relevant sh dp marking selfValue nl <$> getChildren self
    newVis = case self of
      Leaf x -> case Map.lookup x (getMarking marking) of
        Just something -> if initVis /= Hide then initVis else Hide
        Nothing -> if initVis /= Hide then Ask else Hide
      _ -> initVis
    innerfunc self' = case self' of
      Leaf x -> mkQ (newVis) (Simply x) (nlMap x nl) Nothing (fromMaybe (Default $ Left Nothing) (Map.lookup x (getMarking marking))) []
      Not x -> innerfunc x
      Any label items -> mkQ (ask2view initVis) Or (nlMap (label2pre label) nl) (Just label) (Default $ Left selfValue) paintedChildren
      All label items -> mkQ (ask2view initVis) And (nlMap (label2pre label) nl) (Just label) (Default $ Left selfValue) paintedChildren
  in -- convert to a QTree for output
    innerfunc self
  where
  -- from a dictionary of { langID: { shortword: longtext } }
  -- to a dictionary of { langID: longtext }
  nlMap word nldict =
    let
      langs = Set.toUnfoldable $ Map.keys nldict :: Array String
    in
      Map.fromFoldable $ do
        lg <- langs
        let
          lgDict = fromMaybe Map.empty (Map.lookup lg nl)
          longtext = fromMaybe "" (Map.lookup word lgDict)
        pure $ Tuple lg longtext

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

-- well, it depends on what values the children have. and that depends on whether we're assessing them in soft or hard mode.
evaluate :: Hardness -> Marking -> Item String -> Maybe Bool
evaluate Soft (Marking marking) (Leaf x) = case Map.lookup x marking of
  Just (Default (Right (Just y))) -> Just y
  Just (Default (Left (Just y))) -> Just y
  _ -> Nothing
evaluate Hard (Marking marking) (Leaf x) = case Map.lookup x marking of
  Just (Default (Right (Just y))) -> Just y
  _ -> Nothing

evaluate sh marking (Not item) = not <$> (evaluate sh marking item)
evaluate sh marking (Any _ items) = evaluateAny (evaluate sh marking <$> items)
evaluate sh marking (All _ items) = evaluateAll (evaluate sh marking <$> items)

evaluateAny :: forall f. Foldable f => f (Maybe Bool) -> Maybe Bool
evaluateAny items
  | Just true `elem` items = Just true
  | all (_ == Just false) items = Just false
  | otherwise = Nothing

evaluateAll :: forall f. Foldable f => f (Maybe Bool) -> Maybe Bool
evaluateAll items
  | all (_ == Just true) items = Just true
  | Just false `elem` items = Just false
  | otherwise = Nothing

