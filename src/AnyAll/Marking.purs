module AnyAll.Marking
  ( Marking(..)
  , decodeMarkingArgo
  , getMarking
  , markup
  , decodeJsonMarkingArgo
  )
  where

import AnyAll.Ternary (Ternary, dumpDefault)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Foreign.Object
import Partial.Unsafe (unsafeCrashWith)


newtype Marking = Marking (Map.Map String Ternary)

derive instance eqMarking :: Eq (Marking)

derive instance genericMarking :: Generic Marking _

derive newtype instance showMarking :: Show (Marking)

instance encodeJsonMarking :: EncodeJson Marking where
  encodeJson (Marking map) = encodeJson $ fromFoldable (Map.toUnfoldable (dumpDefault <$> map) :: Array _)

jsonToTuples ∷ Object Ternary → Array (Tuple String Ternary)
jsonToTuples = toUnfoldable

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