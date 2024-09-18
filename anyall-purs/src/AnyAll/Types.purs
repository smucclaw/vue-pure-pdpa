module AnyAll.Types(
  module AnyAll.Item,
  module AnyAll.BasicTypes,
  module AnyAll.Marking,
  module AnyAll.Ternary,
  Q(..),
  ShouldView(..),
  AndOr(..),
  mkQ
) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Argonaut.Encode
import Data.Argonaut.Core (Json, jsonEmptyObject)

import AnyAll.Item
import AnyAll.BasicTypes
import AnyAll.Ternary
import AnyAll.Marking

-- together, an Item and a Marking get computed into a tree of Q, which has more structure,
-- and represents the result of and/or shortcutting.
-- a Q tree informs the UI of what to display, what to hide, what to ask for.

newtype Q = Q
  { shouldView :: ShouldView
  , andOr :: AndOr String
  , prePost :: Maybe (Label String)
  , mark :: Ternary
  , children :: Array Q
  }

derive instance eqQ :: Eq (Q)
derive instance genericQ :: Generic (Q) _
instance showQ :: Show (Q) where
  show eta = genericShow eta

mkQ ∷ ShouldView → AndOr String → Maybe (Label String) → Ternary → Array Q → Q
mkQ sv ao pp m c =
  Q
    { shouldView: sv
    , andOr: ao -- slightly different from QoutJS, which contains children in it
    , prePost: pp
    , mark: m
    , children: c
    }

data ShouldView = View | Hide | Ask

derive instance eqShouldView :: Eq (ShouldView)
derive instance genericShouldView :: Generic ShouldView _
instance showShouldView :: Show ShouldView where
  show eta = genericShow eta

instance encodeJsonShouldView :: EncodeJson ShouldView where
  encodeJson a = encodeJson $ shouldViewToString a

shouldViewToString :: ShouldView -> String
shouldViewToString = case _ of
  View -> "View"
  Hide -> "Hide"
  Ask -> "Ask"

instance encodeJsonQ :: EncodeJson Q where
  encodeJson (Q { shouldView, andOr, prePost, mark, children }) =
    "shouldView" := shouldView
      ~> "prePost" := encodePrePostArgo prePost
      ~> "mark" := mark
      ~> "andOr" := encodeAndOrArgo andOr children
      ~> jsonEmptyObject

data AndOr a
  = And -- All
  | Or -- And
  | Simply a -- Leaf

derive instance eqAndOr :: (Eq a) => Eq (AndOr a)
derive instance genericAndOr :: Generic (AndOr a) _
instance showAndOr :: (Show a) => Show (AndOr a) where
  show = genericShow

encodeAndOrArgo :: forall a. EncodeJson a => AndOr a -> Array Q -> Json
encodeAndOrArgo (Simply a) _ = encodeJson $ {  tag : "Leaf" , contents: a, nl: jsonEmptyObject}
encodeAndOrArgo And children = encodeJson $ {  tag : "All", children: encodeJson <$> children, nl: jsonEmptyObject }
encodeAndOrArgo Or children = encodeJson $ {  tag : "Any", children: encodeJson <$> children, nl: jsonEmptyObject }

encodePrePostArgo :: forall a. EncodeJson a => Maybe (Label a) -> Json
encodePrePostArgo (Just x) = encodeJson x
encodePrePostArgo Nothing = jsonEmptyObject