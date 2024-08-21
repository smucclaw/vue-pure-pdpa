module Test.AnyAll.RenderingTest where

import Prelude

import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

import AnyAll.Types
import AnyAll.Relevance (relevant)
import RuleLib.Interview as RuleLib.Interview

import Partial.Unsafe (unsafeCrashWith)
import Data.Map as Map
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Foreign.Generic (Foreign, decode, encode)
import Control.Monad.Except (runExcept)

import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import AnyAll.PdpaQ (pdpaQ2)

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
  [ Tuple "walk" $ Right (Just true)
  , Tuple "run" $ Left (Just true)
  , Tuple "eat" $ Left (Just true)
  , Tuple "drink" $ Right (Just false)
  ]

paintQ :: Marking -> NLDict -> Item String -> Q
paintQ m nl i = relevant Hard DPNormal m Nothing nl i

myq :: Q
myq =
  ( Q
      { andOr: And
      , children:
          [ ( Q
                { andOr: (Simply "walk")
                , children: []
                , mark: (Default (Right (Just true)))
                , prePost: Nothing
                , shouldView: View
                , tagNL: (Map.fromFoldable [ (Tuple "en" "walk slowly") ])
                }
            )
          , ( Q
                { andOr: (Simply "run")
                , children: []
                , mark: (Default (Left (Just true)))
                , prePost: Nothing
                , shouldView: Ask
                , tagNL: (Map.fromFoldable [ (Tuple "en" "run fast") ])
                }
            )
          , ( Q
                { andOr: Or
                , children:
                    [ ( Q
                          { andOr: (Simply "eat")
                          , children: []
                          , mark: (Default (Left (Just true)))
                          , prePost: Nothing
                          , shouldView: Ask
                          , tagNL: (Map.fromFoldable [ (Tuple "en" "eat food") ])
                          }
                      )
                    , ( Q
                          { andOr: (Simply "drink")
                          , children: []
                          , mark: (Default (Right (Just false)))
                          , prePost: Nothing
                          , shouldView: View
                          , tagNL: (Map.fromFoldable [ (Tuple "en" "drink beverages") ])
                          }
                      )
                    ]
                , mark: (Default (Left Nothing))
                , prePost: (Just (Pre "either"))
                , shouldView: View
                , tagNL: (Map.fromFoldable [ (Tuple "en" "") ])
                }
            )
          ]
      , mark: (Default (Left Nothing))
      , prePost: (Just (Pre "all of"))
      , shouldView: View
      , tagNL: (Map.fromFoldable [ (Tuple "en" "") ])
      }
  )

spec :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
spec = describe "Render" do
  it "Example 1" do
    (paintQ marking1 example1_nl example1) `shouldEqual` myq
