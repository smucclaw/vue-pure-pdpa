module AnyAll
  ( anyallform1
  , decodeItemString
  , decodeMarking
  , emptyMarking
  , example1
  , example1_encoded
  , example1_nl
  , exampleAny
  , fromNode1
  , fromNode2
  , fromNode3
  , getItemByName
  , getNLByName
  , hard
  , heads
  , howDoWeEven
  , marking1
  , marking1_decoded
  , marking1_encoded
  , marking1_recoded
  , paint
  , paintQ
  , pdpaQ
  , pdpa_dbno_s1p1
  , pdpa_dbno_s1p1_nl
  , soft
  )
  where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import AnyAll.Types
import AnyAll.Relevance (relevant)
import RuleLib.Interview as RuleLib.Interview

import Partial.Unsafe
import Data.Map as Map
import Data.Either (Either(..), fromRight, either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.List (concatMap, head)
import Data.Foldable (foldMap)
import Foreign.Generic
import Control.Monad.Except
import Foreign
import Record (merge)
import Foreign.Object as Object

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

pdpa_dbno_s1p1 = RuleLib.Interview.interviewRules
pdpa_dbno_s1p1_nl = RuleLib.Interview.interviewRules_nl

emptyMarking = markup Map.empty

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

output1 :: QoutJS
output1 = qoutjs $ output1q

output1q = paintQ marking1 example1_nl example1

pdpaQ = paintQ emptyMarking pdpa_dbno_s1p1_nl pdpa_dbno_s1p1

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

heads x = Object.keys(x)

