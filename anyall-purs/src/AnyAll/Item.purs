module AnyAll.Item(
  Item(..),
  Label(..),
  nnf
) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:!), (.:?), (.!=))
import Data.Maybe (maybe)

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
  encodeJson (Pre x) = encodeJson $ {  pre : x }
  encodeJson (PrePost x y) = encodeJson $ {  pre : x, post: y }

instance decodeJsonLabel :: (DecodeJson a) => DecodeJson (Label a) where
  decodeJson json = do
    obj <- decodeJson json              -- decode `Json` to `Object Json`
    pre <- obj .: "pre"               -- decode the "name" key to a `String`
    post <- obj .:? "post"                -- decode the "age" key to a `Maybe Int`
    pure $ maybe (Pre pre) (PrePost pre) post 