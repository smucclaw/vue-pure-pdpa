module Test.JsonSerde where

import Prelude

import AnyAll.Item (Item, Label(..))
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Monoid (power)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw, Error, error)
import Test.Spec.Assertions (shouldEqual, fail)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)


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
  preJson <- jsonParser' """{ "pre": "Hello" }"""

  prePostJson <- jsonParser' """{ "pre": "Hello", "post": "World"  }"""

  let
    testLabelCases :: Test
    testLabelCases = do
      test "{pre: Hello} should decode to Pre" do
        case decodeJson preJson of
          Right (Pre "Hello") -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify preJson)
      
      test "{pre: Hello, post: World} should decode to PrePost" do
        case decodeJson prePostJson of
          Right (PrePost "Hello" "World") -> pure unit
          _ -> failure ("Failed to properly decode JSON string: " <> stringify prePostJson)

      test "{pre: Hello, post: World} should decode to PrePost" do
        encodeJson (Pre "Helo") `shouldEqualJson` preJson

  suite "Test Label serde" testLabelCases