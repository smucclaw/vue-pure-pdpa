module AnyAll.Relevance where

import AnyAll.Types
import Prelude

import Data.Set as Set
import Data.Tuple
import Data.Map as Map
import Data.List (any, all, elem, List(..))
import Data.Foldable (class Foldable)
import Data.Show (class Show)
import Debug

import Data.Maybe
import Data.Either (Either(..), either)

-- paint a tree as View, Hide, or Ask, depending on the dispositivity of the current node and its children.
relevant :: Hardness -> DisplayPref -> Marking -> Maybe Bool -> NLDict -> Item String -> Q
relevant sh dp marking parentValue nl self =
  let
    selfValue = let result = evaluate sh marking self in trace ("Relevance.evaluate.selfValue: " <> show self <> " = " <> show result) \_ -> result
    initVis =
      if isJust parentValue then
        if parentValue == selfValue then View
        else Hide
      else if isJust (evaluate Hard marking self) then View
      else Ask
    -- we compute the initial visibility of the subtree.
    -- if our initial visibility is to hide, then we mute all our children by converting Ask to Hide; but if any of our children are View, we leave them as View.
    paintedChildren = (if initVis /= Hide then identity else ask2hide) <$> relevant sh dp marking selfValue nl <$> getChildren self
    makeQNode itemNode = case itemNode of
      Leaf x -> mkQ (initVis) (Simply x) (nlMap x nl) Nothing (lookupMarking x marking) []
      Not x -> makeQNode x -- [TODO] we should have a SimplyNot as well
      Any label items -> mkQ (ask2view initVis) Or  (nlMap (label2pre label) nl) (Just label) (Default $ Left selfValue) paintedChildren
      All label items -> trace ("Relevance.evaluate.makeQNode.All " <> show paintedChildren) \_ -> mkQ (ask2view initVis) And (nlMap (label2pre label) nl) (Just label) (Default $ Left selfValue) paintedChildren
    toreturn = makeQNode self
  in -- convert to a QTree for output
   trace ("Relevance.evaluate: with self=" <> show selfValue <> ", returning " <> show toreturn) \_ -> toreturn
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
  Just (Default (Right (Just y))) -> mytrace ("soft evaluate Leaf " <> show x <> ": Right " <> show y) \_ -> Just y
  Just (Default (Left (Just y)))  -> mytrace ("soft evaluate Leaf " <> show x <> ": Left  " <> show y) \_ -> Just y
  _                               -> mytrace ("soft evaluate Leaf " <> show x <> ": Nothing")          \_ -> Nothing

evaluate Hard (Marking marking) (Leaf x) = case Map.lookup x marking of
  Just (Default (Right (Just y))) -> mytrace ("hard evaluate Leaf " <> show x <> ": Right " <> show y) \_ -> Just y
  _ -> Nothing

evaluate sh marking (Not item)    = mytrace ("hard evaluate Not " <> show item <> ": recursing.") \_ -> not <$> (evaluate sh marking item)
evaluate sh marking (Any _ items) = evaluateAny (evaluate sh marking <$> items)
evaluate sh marking (All _ items) = evaluateAll (evaluate sh marking <$> items)

evaluateAny :: Array (Maybe Bool) -> Maybe Bool
evaluateAny items
  | Just true `elem` items      = mytrace ("evaluateAny: returning True   : " <> show items) \_ -> Just true
  | all (_ == Just false) items = mytrace ("evaluateAny: returning False  : " <> show items) \_ -> Just false
  | otherwise                   = mytrace ("evaluateAny: returning Nothing: " <> show items) \_ -> Nothing

evaluateAll :: Array (Maybe Bool) -> Maybe Bool
evaluateAll items
  | all (_ == Just true) items  = mytrace ("evaluateAll: returning True   : " <> show items) \_ -> Just true
  | Just false `elem` items     = mytrace ("evaluateAll: returning False  : " <> show items) \_ -> Just false
  | otherwise                   = mytrace ("evaluateAll: returning Nothing: " <> show items) \_ -> Nothing

lookupMarking :: String -> Marking -> Default Boolean
lookupMarking node marking = fromMaybe (Default $ Left Nothing) (Map.lookup node (getMarking marking))


mytrace :: forall a. String -> (Unit -> a) -> a
mytrace s l = trace s l
