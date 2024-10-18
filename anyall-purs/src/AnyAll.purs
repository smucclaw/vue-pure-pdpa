module AnyAll
  ( module RuleLib.Interview
  , heads
  , paint
  )
  where

import Prelude

import AnyAll.Types
import AnyAll.Relevance (relevant)

import Data.Map as Map
import Foreign.Object(Object, keys)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)

import RuleLib.Interview

paint :: Json -> NLDict -> Item String -> Json
paint fm _ item =
  encodeJson $ relevant (decodeMarkingArgo fm) Unknown item

heads ::  forall t2. Object t2 -> Array String
heads x = keys(x)
