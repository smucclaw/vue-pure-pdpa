module Test.JsonSerde where

import Prelude

import AnyAll.Item (Item(..), Label(..), decodeAajsonGroupItem, decodeAajsonItem)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Monoid (power)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw, Error, error)
import Test.Spec.Assertions (shouldEqual, fail)
import Data.Tuple (Tuple(..))


shouldEqualJson :: forall m . MonadThrow Error m
  => Json
  -> Json
  -> m Unit
shouldEqualJson v1 v2 =
  when (v1 /= v2) $
    fail $ stringify v1 <> " ≠ " <> stringify v2

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
  preJson <- jsonParser' """{ "Pre": "Hello" }"""

  prePostJson <- jsonParser' """{ "Pre": "Hello", "Post": "World"  }"""

  leafItemJson <- jsonParser' """{ "Leaf": "does the person eat?" }"""

  notItemJson <- jsonParser' """{ "Not" : { "Leaf": "does the person eat?" }}"""

  groupItemJson <- jsonParser' """{ "children": [{"Leaf": "does the person eat?"}], "label": {"Pre": "any of:"}}"""

  anyItemJson <- jsonParser' """ {
                        "Any": {
                            "children": [
                                {
                                    "Leaf": "does the person eat?"
                                },
                                {
                                    "Leaf": "does the person drink?"
                                }
                            ],
                            "label": {
                                "Pre": "any of:"
                            }
                        }
                    } """

  let
    testLabelCases :: Test
    testLabelCases = do
      test "{pre: Hello} should decode to Pre" do
        let ff = decodeJson preJson
        case ff of
          Right (Pre "Hello") -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> (show ff))
      
      test "{pre: Hello, post: World} should decode to PrePost" do
        case decodeJson prePostJson of
          Right (PrePost "Hello" "World") -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify prePostJson)

      test "(Pre Hello) should encode to {pre: Hello}" do
        encodeJson (Pre "Hello") `shouldEqualJson` preJson
    
    testItemCases :: Test
    testItemCases = do
      test "{Leaf: does the person eat?} should decode to (Leaf: does the person eat)" do
        let ff = decodeAajsonItem leafItemJson
        case ff of
          Right (Leaf "does the person eat?") -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> (show ff))

      test "{Not: {Leaf: does the person eat?}} should decode to (Not (Leaf: does the person eat?))" do
        let ff = decodeAajsonItem notItemJson
        case ff of
          Right (Not (Leaf "does the person eat?")) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> (show ff))
      
      test "{children: [...], label: {} should decode to (label, children)" do
        let ff = decodeAajsonGroupItem groupItemJson
        case ff of
          Right (Tuple (Pre "any of:") [(Leaf "does the person eat?")]) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> (show ff))

      test "{children: [...], label: {} should decode to (label, children)" do
        let ff = decodeAajsonItem anyItemJson
        case ff of
          Right (Any (Pre "any of:") [(Leaf "does the person eat?"), (Leaf "does the person drink?")]) -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> (show ff))

  suite "Test Label serde" testLabelCases
  suite "Test Item serde" testItemCases