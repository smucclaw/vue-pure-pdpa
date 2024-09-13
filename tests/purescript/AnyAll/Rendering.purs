module Test.AnyAll.RenderingTest where

import Prelude

import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

import AnyAll.Types
import RuleLib.Interview as RuleLib.Interview

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))


import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

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

pdpa_dbno_s1p1 = RuleLib.Interview.interviewRules
pdpa_dbno_s1p1_nl = RuleLib.Interview.interviewRules_nl

emptyMarking :: Marking
emptyMarking = markup Map.empty

marking1 :: Marking
marking1 = markup $ Map.fromFoldable
  [ Tuple "walk" $  True
  , Tuple "run" $  True
  , Tuple "eat" $  True
  , Tuple "drink" $  False
  ]

myq :: Q
myq =
  ( Q
      { andOr: And
      , children:
          [ ( Q
                { andOr: (Simply "walk")
                , children: []
                , mark: True
                , prePost: Nothing
                , shouldView: View
                }
            )
          , ( Q
                { andOr: (Simply "run")
                , children: []
                , mark: True
                , prePost: Nothing
                , shouldView: Ask
                }
            )
          , ( Q
                { andOr: Or
                , children:
                    [ ( Q
                          { andOr: (Simply "eat")
                          , children: []
                          , mark: True
                          , prePost: Nothing
                          , shouldView: Ask
                          }
                      )
                    , ( Q
                          { andOr: (Simply "drink")
                          , children: []
                          , mark: False
                          , prePost: Nothing
                          , shouldView: View
                          }
                      )
                    ]
                , mark: Unknown
                , prePost: (Just (Pre "either"))
                , shouldView: View
                }
            )
          ]
      , mark: Unknown
      , prePost: (Just (Pre "all of"))
      , shouldView: View
      }
  )

spec :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
spec = describe "Render" do
  it "Example 1" do
    1 `shouldEqual` 1
