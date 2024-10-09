module Test.JsonSerde where

import Prelude

import Control.Monad.Gen.Common (genMaybe)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Argonaut.Core (Json, isObject, stringify, toObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Gen (genJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.List (List)
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Monoid (power)
import Data.NonEmpty (NonEmpty)
import Data.String (joinWith)
import Data.String.Gen (genUnicodeString)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign.Object as FO


type Test = ReaderT Int Effect Unit

suite :: String -> Test -> Test
suite = test

test :: String -> Test -> Test
test name run = do
  indent <- ask
  log (mkIndent indent <> name)
  local (_ + 2) run

mkIndent :: Int -> String
mkIndent = power " "


failure :: String -> Test
failure = liftEffect <<< throw

jsonParser' :: String -> ReaderT Int Effect Json
jsonParser' = either (liftEffect <<< throw) pure <<< jsonParser

main :: Effect Unit
main = flip runReaderT 0 do
  suite "Manual Combinators Checks" manualRecordDecode 

manualRecordDecode :: Test
manualRecordDecode = do
  fooJson <- jsonParser' """{ "bar": [1, 2, 3], "baz": true }"""

  fooNestedEmptyJson <- jsonParser' "{ }"

  fooNestedEmptyJsonNull <- jsonParser' """{ "bar": null, "baz": null }"""

  fooNestedBazJson <- jsonParser' """{ "baz": true }"""

  fooNestedBazJsonNull <- jsonParser' """{ "bar": null, "baz": true }"""

  fooNestedBarJson <- jsonParser' """{ "bar": [1] }"""

  fooNestedBarJsonNull <- jsonParser' """{ "bar": [1], "baz": null }"""

  fooNestedFullJson <- jsonParser' """{ "bar": [1], "baz": true }"""

  let
    testEmptyCases :: Test
    testEmptyCases = do
      test "Empty Json should decode to FooNested" do
        case decodeJson fooNestedEmptyJson of
          Right (FooNested { bar: Nothing, baz: false }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedEmptyJson)

      test "Json with null values should fail to decode to FooNested" do
        case decodeJson fooNestedEmptyJsonNull of
          Right (FooNested _) -> failure ("Should have failed to decode JSON string: " <> stringify fooNestedEmptyJsonNull)
          _ -> pure unit

      test "Empty Json should decode to FooNested'" do
        case decodeJson fooNestedEmptyJson of
          Right (FooNested' { bar: Nothing, baz: false }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedEmptyJson)

      test "Json with null values should decode to FooNested'" do
        case decodeJson fooNestedEmptyJsonNull of
          Right (FooNested' { bar: Nothing, baz: false }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedEmptyJsonNull)

    testBarCases :: Test
    testBarCases = do
      test "Missing 'bar' key should decode to FooNested" do
        case decodeJson fooNestedBazJson of
          Right (FooNested { bar: Nothing, baz: true }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedBazJson)

      test "Null 'bar' key should fail to decode to FooNested" do
        case decodeJson fooNestedBazJsonNull of
          Right (FooNested _) -> failure ("Should have failed to decode JSON string: " <> stringify fooNestedBazJsonNull)
          _ -> pure unit

      test "Missing 'bar' key should decode to FooNested'" do
        case decodeJson fooNestedBazJson of
          Right (FooNested' { bar: Nothing, baz: true }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedBazJson)

      test "Null 'bar' key should decode to FooNested'" do
        case decodeJson fooNestedBazJsonNull of
          Right (FooNested' { bar: Nothing, baz: true }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedBazJsonNull)

    testBazCases :: Test
    testBazCases = do
      test "Missing 'baz' key should decode to FooNested" do
        case decodeJson fooNestedBarJson of
          Right (FooNested { bar: Just [ 1 ], baz: false }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedBarJson)

      test "Null 'baz' key should fail to decode to FooNested" do
        case decodeJson fooNestedBarJsonNull of
          Right (FooNested _) -> failure ("Should have failed to decode JSON string: " <> stringify fooNestedBarJsonNull)
          _ -> pure unit

      test "Missing 'baz' key should decode to FooNested'" do
        case decodeJson fooNestedBarJson of
          Right (FooNested' { bar: Just [ 1 ], baz: false }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedBarJson)

      test "Null 'baz' key should decode to FooNested'" do
        case decodeJson fooNestedBarJsonNull of
          Right (FooNested' { bar: Just [ 1 ], baz: false }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedBarJsonNull)

    testFullCases :: Test
    testFullCases = do
      test "Json should decode to FooNested" do
        case decodeJson fooNestedFullJson of
          Right (FooNested { bar: Just [ 1 ], baz: true }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedFullJson)

      test "Json should decode to FooNested'" do
        case decodeJson fooNestedFullJson of
          Right (FooNested { bar: Just [ 1 ], baz: true }) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify fooNestedFullJson)

  test "Test that decoding custom record is pure unitful" do
    case decodeJson fooJson of
      Right (Foo _) -> pure unit
      Left err -> failure $ printJsonDecodeError err

  suite "Test decoding empty record" testEmptyCases
  suite "Test decoding missing 'bar' key" testBarCases
  suite "Test decoding missing 'baz' key" testBazCases
  suite "Test decoding with all fields present" testFullCases


newtype Foo = Foo
  { bar :: Array Int
  , baz :: Boolean
  }

instance decodeJsonFoo :: DecodeJson Foo where
  decodeJson json = do
    x <- decodeJson json
    bar <- x .: "bar"
    baz <- x .: "baz"
    pure $ Foo { bar, baz }

newtype FooNested = FooNested
  { bar :: Maybe (Array Int)
  , baz :: Boolean
  }

instance decodeJsonFooNested :: DecodeJson FooNested where
  decodeJson json = do
    x <- decodeJson json
    bar <- x .:! "bar"
    baz <- x .:! "baz" .!= false
    pure $ FooNested { bar, baz }

newtype FooNested' = FooNested'
  { bar :: Maybe (Array Int)
  , baz :: Boolean
  }

instance decodeJsonFooNested' :: DecodeJson FooNested' where
  decodeJson json = do
    x <- decodeJson json
    bar <- x .:? "bar"
    baz <- x .:? "baz" .!= false
    pure $ FooNested' { bar, baz }

type FooRecord =
  { bar :: Maybe (Array Int)
  }