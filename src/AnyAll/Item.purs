module AnyAll.Item(
  Item(..),
  Label(..),
  nnf,
  decodeIt,
  label2pre,
  label2post
) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Foreign.Generic
import Partial.Unsafe (unsafeCrashWith)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Argonaut.Encode


--
-- the "native" data type represents an And/Or structure as a simple tree of Items
--

data Item a
  = Leaf a
  | All (Label a) (Array (Item a))
  | Any (Label a) (Array (Item a))
  | Not (Item a)



-- | nnf based on the nnf found in dsl/lib/haskell/anyall/src/AnyAll/BoolStruct.hs
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

instance encodeItem :: (Encode a) => Encode (Item a) where
  encode eta = genericEncode defaultOptions eta

instance decodeItem :: (Decode a) => Decode (Item a) where
  decode eta = genericDecode defaultOptions eta

decodeIt :: Foreign -> (Item String)
decodeIt f =
  either
    (\e -> unsafeCrashWith $ "error while decoding Item: " <> show e)
    (\v -> v)
    (runExcept (decode f))

--
-- Item uses Label to prefix a tree with strings like "all of the following" or "any of the below"
--
data Label a
  = Pre a
  | PrePost a a

derive instance eqLabel :: (Eq a) => Eq (Label a)
derive instance genericLabel :: Generic (Label a) _
instance showLabel :: (Show a) => Show (Label a) where
  show = genericShow

instance encodeLabel :: (Encode a) => Encode (Label a) where
  encode eta = genericEncode defaultOptions eta

instance decodeLabel :: (Decode a) => Decode (Label a) where
  decode eta = genericDecode defaultOptions eta

instance encodeJsonLabel :: (EncodeJson a) => EncodeJson (Label a) where
  encodeJson (Pre x) = encodeJson $ {  pre : x }
  encodeJson (PrePost x y) = encodeJson $ {  pre : x, post: y }

label2pre ∷ Label String → String
label2pre (Pre x) = x
label2pre (PrePost x _) = x

label2post ∷ Label String → String
label2post (PrePost _ y) = y
label2post (Pre _) = "" -- maybe throw an error?
