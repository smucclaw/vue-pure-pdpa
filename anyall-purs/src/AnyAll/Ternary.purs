module AnyAll.Ternary(
  Ternary(..),
  ternary2string,
  dumpDefault,
  not3
) where

import Prelude

import AnyAll.BasicTypes (DefaultRecord)

import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode

data Ternary = True | False | Unknown

derive instance eqTernary :: Eq (Ternary)
derive instance genericDefault :: Generic (Ternary) _
instance showDefault :: Show (Ternary) where
  show = genericShow

ternary2string ∷ Ternary → String
ternary2string True = "true"
ternary2string False = "false"
ternary2string Unknown = "undefined"

ternaryFromString :: String -> Ternary
ternaryFromString = case _ of
  "true" -> True
  "false" -> False
  _ -> Unknown

not3 ∷ Ternary → Ternary
not3 True = False
not3 False = True
not3 Unknown = Unknown

dumpDefault ∷ Ternary → DefaultRecord
dumpDefault x = { source: "user", value: ternary2string x }

instance encodeJsonTernary :: EncodeJson Ternary where
  encodeJson a = encodeJson $ { source: "user", value: ternary2string a }

instance decodeJsonTernary :: DecodeJson Ternary where
  decodeJson json = do
    obj <- decodeJson json
    value <- obj .: "value"
    note (TypeMismatch "Ternary") (Just $ ternaryFromString value)