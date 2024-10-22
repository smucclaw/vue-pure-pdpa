module AnyAll.Item
  ( Item(..)
  , Label(..)
  , decodeAajsonGroupItem
  , decodeAajsonItem
  , decodeItemArgo
  , nnf
  )
  where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Argonaut.Decode.Decoders (getFieldOptional', decodeArray, getField)
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

data Item a
  = Leaf a
  | All (Label a) (Array (Item a))
  | Any (Label a) (Array (Item a))
  | Not (Item a)


nnf :: forall a. Item a -> Item a
nnf (Not (Not p)) = nnf p
nnf (Not (All l ps)) = Any l $ nnf <$> Not <$> ps
nnf (Not (Any l ps)) = All l $ nnf <$> Not <$> ps
nnf (All l ps) = All l (nnf <$> ps)
nnf (Any l ps) = Any l (nnf <$> ps)
nnf x = x


-- boilerplate for class derivations
derive instance eqItem :: (Eq a) => Eq (Item a)
derive instance genericItem :: Generic (Item a) _
instance showItem :: (Show a) => Show (Item a) where
  show eta = genericShow eta

data Label a
  = Pre a
  | PrePost a a

derive instance eqLabel :: (Eq a) => Eq (Label a)
derive instance genericLabel :: Generic (Label a) _
instance showLabel :: (Show a) => Show (Label a) where
  show = genericShow

instance encodeJsonLabel :: (EncodeJson a) => EncodeJson (Label a) where
  encodeJson (Pre x) = encodeJson $ {  "Pre" : x }
  encodeJson (PrePost x y) = encodeJson $ {  "Pre" : x, "Post": y }

instance decodeJsonLabel :: (DecodeJson a) => DecodeJson (Label a) where
  decodeJson json = do
    obj <- decodeJson json              -- decode `Json` to `Object Json`
    pre <- obj .: "Pre"               -- decode the "name" key to a `String`
    post <- obj .:? "Post"                -- decode the "age" key to a `Maybe Int`
    pure $ maybe (Pre pre) (PrePost pre) post

decodeAajsonItem :: Json -> Either JsonDecodeError (Item String)
decodeAajsonItem json = do
  obj <- decodeJson json
  leafValue <- obj .:? "Leaf"
  notValue <- getFieldOptional' decodeAajsonItem obj "Not"
  anyValue <- getFieldOptional' decodeAajsonGroupItem obj "Any"
  allValue <- getFieldOptional' decodeAajsonGroupItem obj "All"
  pure $ maybe (maybe (maybe (maybe (Leaf "default") (itemFromTuple All) allValue) (itemFromTuple Any) anyValue) Not notValue) Leaf leafValue

itemFromTuple:: (Label String → Array (Item String) → Item String) -> Tuple (Label String) (Array (Item String)) -> Item String
itemFromTuple ic (Tuple l c) = ic l c

decodeAajsonGroupItem :: Json -> Either JsonDecodeError (Tuple (Label String) (Array (Item String)))
decodeAajsonGroupItem json = do
  obj <- decodeJson json
  label <- obj .: "label"
  notValue <- getField (decodeArray decodeAajsonItem) obj "children"
  pure $ Tuple label notValue


decodeItemArgo :: Json -> (Item String)
decodeItemArgo json = case decodeAajsonItem json of
      Right m -> m
      Left jde -> unsafeCrashWith ("Failed to properly decode JSON string: " <> show (jde))