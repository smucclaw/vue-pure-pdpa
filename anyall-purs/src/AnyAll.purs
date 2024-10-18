module AnyAll
  ( module RuleLib.Interview
  , paint
  , paint2
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

paint :: Json -> Item String -> Json
paint fm item =
  encodeJson $ relevant (decodeMarkingArgo fm) Unknown item

paint2 :: Json -> Json -> Json
paint2 fm item =
  encodeJson $ relevant (decodeMarkingArgo fm) Unknown (decodeItemArgo item)