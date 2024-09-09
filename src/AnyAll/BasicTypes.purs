module AnyAll.BasicTypes(
  NLDict,
  DefaultRecord,
  maybe2string
) where


import Data.Maybe

import Data.Map as Map
import Prelude (class Show, show)

type NLDict = Map.Map String (Map.Map String String)

maybe2string ∷ ∀ a. Show a ⇒ Maybe a → String
maybe2string (Just x) = show x
maybe2string Nothing = "undefined"

type DefaultRecord =
  { source :: String
  , value :: String
  }