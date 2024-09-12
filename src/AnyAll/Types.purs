module AnyAll.Types(
  module AnyAll.Item,
  module AnyAll.BasicTypes,
  module AnyAll.Marking,
  module AnyAll.Ternary,
  Q(..),
  QoutJS(..),
  PrePostRecord(..),
  ShouldView(..),
  AndOr(..),
  mkQ,
  qoutjs
) where

import Prelude


import Data.Traversable (sequence)
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.String
import Data.String as DString
import Data.Symbol
import Data.Map as Map
import Option as Option
import Simple.JSON as JSON

import Partial.Unsafe
import Data.List
import Control.Monad.Except
import Foreign
import Foreign.Index ((!), readProp)
import Foreign.Keys as FK
import Foreign.Object as FO
import Foreign.Generic
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Data.Argonaut.Encode
import Data.Argonaut.Core

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
  , tagNL :: Map.Map String String
  , prePost :: Maybe (Label String)
  , mark :: Ternary
  , children :: Array Q
  }

derive instance eqQ :: Eq (Q)
derive instance genericQ :: Generic (Q) _
instance showQ :: Show (Q) where
  show eta = genericShow eta

-- instance encodeQ :: Encode (Q) where
--   encode (Q { shouldView, andOr, tagNL, prePost, mark, children }) =
--     genericEncode defaultOptions { shouldView, andOr, prePost, mark, children }
-- and then do something about the tagNL map

-- it would be nice to use record wildcard constructors but i can't seem to figure it out.
-- https://github.com/purescript/documentation/blob/master/language/Records.md
-- I tried mkQ = Q <<< { shouldView: _, ... }
mkQ sv ao nl pp m c =
  Q
    { shouldView: sv
    , andOr: ao -- slightly different from QoutJS, which contains children in it
    , tagNL: nl
    , prePost: pp
    , mark: m
    , children: c
    }

newtype QoutJS = QoutJS
  ( Option.Option
      ( shouldView :: String
      , andOr ::
          Option.Option
            ( tag :: String
            , nl :: FO.Object String
            , contents :: String
            , children :: Array QoutJS
            )
      , prePost :: PrePostRecord
      , post :: String
      , mark :: DefaultRecord
      )
  )

derive instance eqQoutJS :: Eq QoutJS
derive instance genericQoutJS :: Generic QoutJS _
instance showQoutJS :: Show QoutJS where
  show eta = genericShow eta

qoutjs :: Q -> QoutJS
qoutjs (Q { shouldView, andOr, tagNL, prePost, mark, children }) =
  QoutJS $ Option.fromRecord
    { shouldView: show shouldView
    , andOr: case andOr of
        And -> Option.fromRecord { tag: "All", children: qoutjs <$> children, nl: miniNL }
        Or -> Option.fromRecord { tag: "Any", children: qoutjs <$> children, nl: miniNL }
        (Simply x) -> Option.fromRecord
          { tag: "Leaf"
          , contents: Just x
          , nl: miniNL
          }
    , prePost: dumpPrePost prePost
    , mark: dumpDefault mark
    }
  where
  miniNL =
    FO.fromFoldable (Map.toUnfoldable tagNL :: Array (Tuple String String))

newtype PrePostRecord = PPR (Option.Option (pre :: String, post :: String))

derive instance eqPrePostRecord :: Eq PrePostRecord
derive instance genericPrePostRecord :: Generic PrePostRecord _
instance showPrePostRecord :: Show PrePostRecord where
  show eta = genericShow eta

instance encodePrePostRecord :: Encode PrePostRecord where
  encode eta = unsafeToForeign eta

dumpPrePost :: Maybe (Label String) -> PrePostRecord
dumpPrePost (Just (Pre x)) = PPR $ Option.fromRecord { pre: x }
dumpPrePost (Just (PrePost x y)) = PPR $ Option.fromRecord { pre: x, post: y }
dumpPrePost (Nothing) = PPR $ Option.empty

data ShouldView = View | Hide | Ask

derive instance eqShouldView :: Eq (ShouldView)
derive instance genericShouldView :: Generic ShouldView _
instance showShouldView :: Show ShouldView where
  show eta = genericShow eta

instance encodeShouldView :: Encode ShouldView where
  encode eta = genericEncode defaultOptions eta


instance encodeJsonShouldView :: EncodeJson ShouldView where
  encodeJson a = encodeJson $ shouldViewToString a

shouldViewToString :: ShouldView -> String
shouldViewToString = case _ of
  View -> "View"
  Hide -> "Hide"
  Ask -> "Ask"

instance encodeJsonQ :: EncodeJson Q where
  encodeJson (Q { shouldView, andOr, tagNL, prePost, mark, children }) =
    "shouldView" := shouldView
      ~> "prePost" := jsonEmptyObject
      ~> "mark" := mark
      ~> jsonEmptyObject

data AndOr a
  = And -- All
  | Or -- And
  | Simply a -- Leaf

derive instance eqAndOr :: (Eq a) => Eq (AndOr a)
derive instance genericAndOr :: Generic (AndOr a) _
instance showAndOr :: (Show a) => Show (AndOr a) where
  show = genericShow

instance encodeAndOr :: (Encode a) => Encode (AndOr a) where
  encode eta = genericEncode defaultOptions eta

instance encodeJsonAndOr :: (EncodeJson a) => EncodeJson (AndOr a) where
  encodeJson (Simply a) = encodeJson $ {  tag : "Leaf" }
  encodeJson And = encodeJson $ {  tag : "Leaf" }
  encodeJson Or = encodeJson $ {  tag : "Leaf" }