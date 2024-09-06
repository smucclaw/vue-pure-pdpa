module AnyAll.Marking(
  Marking(..),
  Default(..),
  DefaultRecord,
  getMarking,
  markup,
  dumpDefault
) where

import Prelude


import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.List  (List)
import Foreign (readString, unsafeToForeign)
import Foreign.Index ((!), readProp)
import Foreign.Keys as FK
import Foreign.Object as FO
import Foreign.Generic  (class Decode, class Encode, encode)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import AnyAll.BasicTypes  (maybe2string)

newtype Marking = Marking (Map.Map String (Default Boolean))

derive instance eqMarking :: Eq (Marking)
derive instance genericMarking :: Generic Marking _
derive newtype instance showMarking :: Show (Marking)
instance encodeMarking :: Encode Marking where
  encode (Marking mymap) = unsafeToForeign $ FO.fromFoldable (Map.toUnfoldable (dumpDefault <$> mymap) :: List _)

instance decodeMarking :: Decode Marking where
  decode fm = do
    mkeys <- FK.keys fm
    astuples <- sequence $ (readDefault fm <$> mkeys)
    pure $ markup $ Map.fromFoldable astuples

-- there's a tutorial about how to deal with undefined but for now we are just going to go with string values of Maybe Bool
readDefault fm mk = do
  source <- (fm ! mk) >>= readProp "source" >>= readString
  value <- (fm ! mk) >>= readProp "value" >>= readString
  let
    lr = case source of
      "default" -> Left
      "user" -> Right
      _ -> Left
    mb =
      case value of
        "true" -> Just true
        "false" -> Just false
        "undefined" -> Nothing
        _ -> Nothing
  pure $ Tuple mk mb

markup :: Map.Map String (Maybe Boolean) -> Marking
markup x = Marking $ Default <$> x

getMarking ∷ Marking → Map.Map String (Default Boolean)
getMarking (Marking mymap) = mymap

newtype Default a = Default (Maybe a)

derive instance eqDefault :: (Eq a) => Eq (Default a)
derive instance genericDefault :: Generic (Default a) _
instance showDefault :: (Show a) => Show (Default a) where
  show = genericShow

instance encodeDefault :: (Show a, Encode a) => Encode (Default a) where
  encode eta = encode $ dumpDefault (eta)

dumpDefault ∷ ∀ a. Show a ⇒ Default a → DefaultRecord
dumpDefault (Default x) = { source: "user", value: maybe2string x }

type DefaultRecord =
  { source :: String
  , value :: String
  }

