module AnyAll
  ( hard
  , heads
  , paint
  , soft
  , emptyMarking
  )
  where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import AnyAll.Types
import AnyAll.Relevance (relevant)
import RuleLib.Interview as RuleLib.Interview

import Partial.Unsafe
import Data.Map as Map
import Data.Either (Either(..), fromRight, either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.List (concatMap, head)
import Data.Foldable (foldMap)
import Foreign.Generic
import Control.Monad.Except
import Foreign
import Record (merge)
import Foreign.Object as Object

main = log "AnyAll main"

hard = Hard
soft = Soft

emptyMarking = markup Map.empty

decodeMarking :: Foreign -> Marking
decodeMarking marking =
  let
    eitherm = runExcept $ decode marking
  in
    either
      (\e -> unsafeCrashWith $ "error in decodeMarking" <> show e)
      (\m -> m)
      eitherm

paint :: Hardness -> Foreign -> NLDict -> Item String -> QoutJS
paint h fm nl item =
  qoutjs $ relevant h DPNormal (decodeMarking fm) Nothing nl item

heads x = Object.keys(x)
