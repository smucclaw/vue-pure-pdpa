module AnyAll.Ternary(
  Ternary(..),
  ternary2string,
  dumpDefault,
  readDefault,
  not3
) where

import Prelude

import AnyAll.BasicTypes

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Foreign (readString, unsafeToForeign, ForeignError)
import Foreign.Generic (class Decode, class Encode, encode)
import Foreign.Index ((!), readProp, class Index, class Indexable)
import Control.Monad.Except.Trans
import Data.Argonaut.Encode

data Ternary = True | False | Unknown

derive instance eqTernary :: Eq (Ternary)
derive instance genericDefault :: Generic (Ternary) _
instance showDefault :: Show (Ternary) where
  show = genericShow

ternary2string ∷ Ternary → String
ternary2string True = "true"
ternary2string False = "false"
ternary2string Unknown = "undefined"

not3 ∷ Ternary → Ternary
not3 True = False
not3 False = True
not3 Unknown = Unknown

dumpDefault ∷ Ternary → DefaultRecord
dumpDefault x = { source: "user", value: ternary2string x }

instance encodeJsonTernary :: EncodeJson Ternary where
  encodeJson a = encodeJson $ { source: "user", value: ternary2string a }

readDefault fm mk = do
  source <- (fm ! mk) >>= readProp "source" >>= readString
  value <- (fm ! mk) >>= readProp "value" >>= readString
  let
    lr = case source of
      "default" -> Left
      "user" -> Right
      _ -> Left
    mb =
      case value of
        "true" -> True
        "false" -> False
        "undefined" -> Unknown
        _ -> Unknown
  pure $ Tuple mk mb