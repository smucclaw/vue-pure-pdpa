
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

import Data.Maybe
import Data.Either (Either(..), either)

-- paint a tree as View, Hide, or Ask, depending on the dispositivity of the current node and its children.
relevant :: Hardness -> DisplayPref -> Marking -> Maybe Bool -> NLDict -> Item String -> Q
relevant sh dp marking parentValue nl self =
  let selfValue = evaluate sh marking self
      initVis   = if isJust parentValue then if parentValue == selfValue              then View
                                                                                      else Hide
                                        else if isJust (evaluate Hard marking self)   then View
                                                                                      else Ask
      -- we compute the initial visibility of the subtree.
      -- if our initial visibility is to hide, then we mute all our children by converting Ask to Hide; but if any of our children are View, we leave them as View.
      paintedChildren = (if initVis /= Hide then identity else ask2hide) <$> relevant sh dp marking selfValue nl <$> getChildren self
      newVis = case self of
        Leaf x -> case Map.lookup x (getMarking marking) of
          Just something -> if initVis /= Hide then initVis else Hide
          Nothing        -> if initVis /= Hide then Ask     else Hide
        _ -> initVis
  in -- convert to a QTree for output
  case self of
    Leaf x          -> mkQ (newVis)    (Simply x) (nlMap x nl)                  Nothing     (fromMaybe (Default $ Left Nothing) (Map.lookup x (getMarking marking))) []
    Any label items -> mkQ (ask2view initVis)  Or (nlMap (label2pre label) nl) (Just label)            (Default $ Left selfValue)                       paintedChildren
    All label items -> mkQ (ask2view initVis) And (nlMap (label2pre label) nl) (Just label)            (Default $ Left selfValue)                       paintedChildren
  where
    -- from a dictionary of { langID: { shortword: longtext } }
    -- to a dictionary of { langID: longtext }
    nlMap word nldict =
      let langs = Set.toUnfoldable $ Map.keys nldict :: Array String
      in Map.fromFoldable $ do
            lg <- langs
            let lgDict = fromMaybe Map.empty (Map.lookup lg nl)
                longtext = fromMaybe "" (Map.lookup word lgDict)
            pure $ Tuple lg longtext

    getChildren (Leaf _) = []
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
                                             Just (Default (Left  (Just y))) -> Just y
                                             _                               -> Nothing
evaluate Hard (Marking marking) (Leaf x) = case Map.lookup x marking of
                                             Just (Default (Right (Just y))) -> Just y
                                             _                               -> Nothing
evaluate sh marking (Any label items)
  | Just true `elem`      (evaluate sh marking <$> items) = Just true
  | all (_ == Just false) (evaluate sh marking <$> items) = Just false
  | otherwise = Nothing
evaluate sh marking (All label items)
  | all (_ == Just true) (evaluate sh marking <$> items) = Just true
  | Just false `elem`    (evaluate sh marking <$> items) = Just false
  | otherwise = Nothing

