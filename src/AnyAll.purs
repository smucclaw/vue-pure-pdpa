module AnyAll
  ( fromNode1
  , fromNode2
  , fromNode3
  , emptyMarking
  , example1_nl
  , example1
  , exampleAny
  , example1_encoded
  , marking1
  , marking1_encoded
  , marking1_decoded
  , marking1_recoded
  , anyallform1
  , decodeMarking
  , pdpa_dbno_s1p1
  , pdpa_dbno_s1p1_nl
  , pdpaQ
  , paint
  , paintQ
  , hard
  , soft
  , howDoWeEven
  , getItemByName
  , getNLByName
  , decodeItemString
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import AnyAll.Types
import AnyAll.Relevance (relevant)
import RuleLib.PDPADBNO as RuleLib.PDPADBNO

import Partial.Unsafe
import Data.Map as Map
import Data.Either (Either(..), fromRight, either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.List (concatMap)
import Foreign.Generic
import Control.Monad.Except
import Foreign

main = log "AnyAll main"

hard = Hard
soft = Soft

fromNode1 :: String
fromNode1 = "hello node"

fromNode2 :: String -> String
fromNode2 x = "hello node, you said " <> x

fromNode3 :: QoutJS
fromNode3 = output1

example1 :: Item String
example1 =
  ( All (Pre "all of")
      [ Leaf "walk"
      , Leaf "run"
      , Any (Pre "either")
          [ Leaf "eat"
          , Leaf "drink"
          ]
      ]
  )

exampleAny :: Item String
exampleAny =
  ( Any (Pre "all of")
      [ Leaf "walk"
      , Leaf "run"
      ]
  )

example1_nl :: NLDict
example1_nl =
  Map.fromFoldable
    [ Tuple "en" $ Map.fromFoldable
        [ Tuple "walk" "walk slowly"
        , Tuple "run" "run fast"
        , Tuple "eat" "eat food"
        , Tuple "drink" "drink beverages"
        ]
    ]

example1_encoded = encode example1

pdpa_dbno_s1p1 :: Item String
pdpa_dbno_s1p1 = RuleLib.PDPADBNO.schedule1_part1
pdpa_dbno_s1p1_nl :: NLDict
pdpa_dbno_s1p1_nl = RuleLib.PDPADBNO.schedule1_part1_nl

emptyMarking :: Marking
emptyMarking = markup Map.empty

-- q: what does markup do?
-- a: it takes a map from strings to eithers of maybes of bools, and returns a marking
-- q: what is a marking?
-- a: it's a map from strings to eithers of maybes of bools
-- q: what is Map.fromFoldable?
-- a: it's a function that takes a list of tuples and returns a map
-- q: how does Map.fromFoldable work?
-- a: it takes a list of tuples, and for each tuple, it adds a key-value pair to the map
-- q: what are the keys and values?
-- a: the keys are the first elements of the tuples, and the values are the second elements of the tuples
marking1 :: Marking
marking1 = markup $ Map.fromFoldable
  [ Tuple "walk" $ Right (Just true)
  , Tuple "run" $ Left (Just true)
  , Tuple "eat" $ Left (Just true)
  , Tuple "drink" $ Right (Just false)
  ]

marking1_encoded = encode marking1

decodeMarking :: Foreign -> Marking
decodeMarking marking =
  let
    eitherm = runExcept $ decode marking
  in
    either
      (\e -> unsafeCrashWith $ "error in decodeMarking" <> show e)
      (\m -> m)
      eitherm

marking1_decoded = decodeMarking marking1_encoded

marking1_recoded x = decodeMarking $ encode x

-- q: where is the function qoutjs defined?
-- a: in AnyAll.Types
output1 :: QoutJS
output1 = qoutjs $ output1q

-- q: what does output1q do?
-- a: it takes a marking, a nldict, and a rule item, and returns a qout
-- q: what is a nldict?
-- a: it's a map from strings to maps from strings to strings
-- q: where is nldict defined?
-- a: in AnyAll.Types
-- q: why should output1q have a type signature of Qout?
-- a: because it's called from javascript
-- q: how do we run output1q from the terminal?
-- a: we can't
-- q: how do we test and print output1q?
-- a: we can't
output1q :: Qout
output1q = paintQ marking1 example1_nl example1

-- q: what does pdpaQ do?
-- a: it takes a marking, a nldict, and a rule item, and returns a qoutjs
pdpaQ :: QoutJS
pdpaQ = paintQ emptyMarking pdpa_dbno_s1p1_nl pdpa_dbno_s1p1

-- q: what does paintQ do?
-- a: it takes a marking, a nldict, and a rule item, and returns a qout
-- q: when does it return a quotjs?
-- a: when it's called from pdpaQ
-- q: why does it return a quotjs when it's called from pdpaQ?
-- a: because pdpaQ is called from javascript
-- q: how do we know pdpaQ is called from javascript?
-- a: because it's called from the main function in index.js

-- q: explain `relevant Hard DPNormal m Nothing nl i`.
-- a: it takes a hardness, a display preference, a marking, a maybe, a nldict, and a rule item, and returns a qout
-- q: what is display preference?
-- a: it's a data type defined in AnyAll.Types
-- q: what is a rule item?
-- a: it's a data type defined in AnyAll.Types
paintQ :: Marking -> NLDict -> Item String -> Qout
paintQ m nl i = relevant Hard DPNormal m Nothing nl i

anyallform1 = output1

type ItemName = String

-- todo -- reorganize these
itemLibrary = Map.fromFoldable
  [ Tuple "example1" example1
  , Tuple "pdpa_dbno_s1p1" pdpa_dbno_s1p1
  ]

nlLibrary = Map.fromFoldable
  [ Tuple "example1" example1_nl
  , Tuple "pdpa_dbno_s1p1" pdpa_dbno_s1p1_nl
  ]

paint :: Hardness -> Foreign -> NLDict -> Item String -> QoutJS
paint h fm nl item =
  qoutjs $ relevant h DPNormal (decodeMarking fm) Nothing nl item

getItemByName :: String -> Item String
getItemByName itemname =
  case Map.lookup itemname itemLibrary of
    Nothing -> unsafeCrashWith $ "anyall: unable to find rule item named " <> itemname
    (Just item) -> item

getNLByName :: String -> NLDict
getNLByName itemname =
  case Map.lookup itemname nlLibrary of
    Nothing -> unsafeCrashWith $ "anyall: unable to find nldict named " <> itemname
    (Just item) -> item

howDoWeEven :: String -> Int -> String
howDoWeEven arg1 arg2 = "arg 1 = " <> arg1 <> "; arg 2 = " <> show arg2

decodeItemString = decodeIt

