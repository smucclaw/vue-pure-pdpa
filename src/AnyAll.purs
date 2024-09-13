module AnyAll
  ( heads
  , paint
  , emptyMarking
  , decodeMarking
  )
  where

import Prelude

import AnyAll.Types
import AnyAll.Relevance (relevant)

import Partial.Unsafe (unsafeCrashWith)
import Data.Map as Map
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Foreign.Generic  (Foreign, decode)
import Control.Monad.Except (runExcept)
import Foreign.Object(Object, keys)

import Data.Argonaut.Core
import Data.Argonaut.Encode


emptyMarking :: Marking
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

paint :: Json -> NLDict -> Item String -> Json
paint fm _ item =
  encodeJson $ relevant (decodeMarkingArgo fm) Unknown item

heads ::  forall t2. Object t2 -> Array String
heads x = keys(x)
