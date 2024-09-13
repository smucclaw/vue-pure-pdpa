module AnyAll.Marking
  ( Marking(..)
  , decodeMarkingArgo
  , getMarking
  , markup
  , decodeJsonMarkingArgo
  )
  where

import AnyAll.Ternary
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Either
import Data.Tuple
import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map as Map
import Data.Set (Set)
import Data.Traversable (sequence, traverse)
import Foreign (readString, unsafeToForeign)
import Foreign.Generic (class Decode, class Encode, encode)
import Foreign.Keys as FK
import Foreign.Object (Object)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Data.Unfoldable


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

jsonToTuples ∷ Object Ternary → Array (Tuple String Ternary)
jsonToTuples = FO.toUnfoldable

jsonAsMap ∷ Object Ternary → Map.Map String Ternary
jsonAsMap = Map.fromFoldable <<< jsonToTuples

markup :: Map.Map String Ternary -> Marking
markup x = Marking $ x

getMarking ∷ Marking → Map.Map String Ternary
getMarking (Marking mymap) = mymap

decodeJsonMarkingArgo :: Json -> Either JsonDecodeError Marking
decodeJsonMarkingArgo json =  do
    obj <- decodeJson json
    pure $ Marking $ jsonAsMap obj

decodeMarkingArgo :: Json -> Marking
decodeMarkingArgo json = case decodeJsonMarkingArgo json of
      Right m -> m
      Left jde -> unsafeCrashWith ("Failed to properly decode JSON string: " <> show (jde))