module Test.AnyAll.ItemTest
  ( spec
  )
  where

import AnyAll.Types
import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Gen.Common (genMaybe)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Argonaut.Core (Json, isObject, stringify, toObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Gen (genJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array.NonEmpty (NonEmptyArray)
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
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (throw)
import Foreign.Object as FO
import RuleLib.Interview as RuleLib.Interview
import Test.AnyAll.RenderingTest (myq)
import Test.Spec (SpecT, Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


myq fooJson = case decodeJson fooJson of
              Right (l@(Pre _)::Label String) -> l
              Right ((PrePost _ _)::Label String) -> (Pre "dummy")
              Left err -> (Pre "kaput")

myq2 jsonString = case jsonParser jsonString of
          (Right fooJson) -> (Pre "dummy")
          (Left err) -> (Pre err)

spec ::  Spec Unit
spec = describe "hide view" do
  describe "Hard" do
    it "Self Just T / Parent Just T" do
      let r = myq2 """{ "pre": "MyLabel" }"""
      r `shouldEqual` (Pre "dummy")
