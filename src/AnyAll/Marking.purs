module AnyAll.Marking(
  Marking(..),
  getMarking,
  markup
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
import AnyAll.Ternary

newtype Marking = Marking (Map.Map String Ternary)

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


markup :: Map.Map String Ternary -> Marking
markup x = Marking $ x

getMarking ∷ Marking → Map.Map String Ternary
getMarking (Marking mymap) = mymap

