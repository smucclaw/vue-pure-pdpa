module AnyAll.Types where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Traversable (sequence)
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.String
import Data.String   as DString
import Data.Symbol
import Data.Map      as Map
import Option        as Option
import Simple.JSON   as JSON

import Partial.Unsafe
import Data.List
import Control.Monad.Except
import Foreign
import Foreign.Index ((!), readProp)
import Foreign.Keys   as FK
import Foreign.Object as FO
import Foreign.Generic
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type Bool = Boolean

--
-- the "native" data type represents an And/Or structure as a simple tree of Items
--

data Item a =
    Leaf a
  | All (Label a) (Array (Item a))
  | Any (Label a) (Array (Item a))

-- boilerplate for class derivations
derive instance  eqItem :: (Eq a) => Eq (Item a)
derive instance genericItem :: Generic (Item a) _
instance showItem :: (Show a) => Show (Item a) where show eta = genericShow eta
instance encodeItem :: (Encode a) => Encode (Item a) where encode eta = genericEncode defaultOptions eta
instance decodeItem :: (Decode a) => Decode (Item a) where decode eta = genericDecode defaultOptions eta

decodeIt :: Foreign -> (Item String)
decodeIt f =
  either
  (\e -> unsafeCrashWith $ "error while decoding Item: " <> show e)
  (\v -> v)
  (runExcept (decode f))
--
-- Item uses Label to prefix a tree with strings like "all of the following" or "any of the below"
--
data Label a = Pre a
             | PrePost a a
derive instance  eqLabel :: (Eq a) => Eq (Label a)
derive instance genericLabel :: Generic (Label a) _
instance showLabel :: (Show a) => Show (Label a) where show = genericShow
instance encodeLabel :: (Encode a) => Encode (Label a) where encode eta = genericEncode defaultOptions eta
instance decodeLabel :: (Decode a) => Decode (Label a) where decode eta = genericDecode defaultOptions eta

label2pre (Pre x) = x
label2pre (PrePost x y) = x
label2post (PrePost x y) = y
label2post (Pre x) = "" -- maybe throw an error?

type NLDict = Map.Map String (Map.Map String String)

-- an Item tree represents the logic. The logic is immutable, at least within the short-term lifetime of a user session.
-- By contrast, a Marking contains the current state of which elements have received user input;
-- if no user input was received, the Marking gives default values. This gets updated every time the user clicks something.

newtype Marking = Marking (Map.Map String (Default Bool))
derive instance  eqMarking :: Eq (Marking)
derive instance genericMarking :: Generic Marking _
derive newtype instance showMarking :: Show (Marking)
instance encodeMarking :: Encode Marking where
  encode (Marking mymap) = unsafeToForeign $ FO.fromFoldable (Map.toUnfoldable (dumpDefault <$> mymap) :: List _)

-- should this be decodeMarking?
instance decodeMarking :: Decode Marking where
  decode fm = do
    mkeys <- FK.keys fm
    astuples <- sequence $ (readDefault fm <$> mkeys)
    pure $ markup $ Map.fromFoldable astuples


-- there's a tutorial about how to deal with undefined but for now we are just going to go with string values of Maybe Bool
readDefault fm mk = do
  source <- (fm ! mk) >>= readProp "source" >>= readString
  value  <- (fm ! mk) >>= readProp "value"  >>= readString
  let lr = case source of
          "default" -> Left
          "user"    -> Right
          _         -> Left
      mb =
        case value of
          "true"      -> Just true
          "false"     -> Just false
          "undefined" -> Nothing
          _           -> Nothing
  pure $ Tuple mk (lr mb)

markup :: Map.Map String (Either (Maybe Boolean) (Maybe Boolean)) -> Marking
markup x = Marking $ Default <$> x

getMarking (Marking mymap) = mymap

newtype Default a = Default (Either (Maybe a) (Maybe a))
derive instance  eqDefault :: (Eq a) => Eq (Default a)
derive instance genericDefault :: Generic (Default a) _
instance showDefault :: (Show a) => Show (Default a) where show = genericShow
instance encodeDefault :: (Show a, Encode a) => Encode (Default a) where
  encode eta = encode $ dumpDefault (eta)
  

dumpDefault :: forall a. Show a => Default a -> DefaultRecord
dumpDefault (Default ( Left x))  = { source: "default", value: maybe2string x }
dumpDefault (Default (Right x))  = { source: "user",    value: maybe2string x }

type DefaultRecord = { source :: String
                     , value :: String }


-- together, an Item and a Marking get computed into a tree of Q, which has more structure,
-- and represents the result of and/or shortcutting.
-- a Q tree informs the UI of what to display, what to hide, what to ask for.

newtype Q = Q { shouldView :: ShouldView
              , andOr      :: AndOr String
              , tagNL      :: Map.Map String String
              , prePost    :: Maybe (Label String)
              , mark       :: Default Bool
              , children   :: Array Q
              }
derive instance  eqQ :: Eq (Q)
derive instance genericQ :: Generic (Q) _
instance showQ :: Show (Q) where show eta = genericShow eta

type R = { shouldView :: ShouldView
         , andOr      :: AndOr String
         , tagNL      :: Map.Map String String
         , prePost    :: Maybe (Label String)
         , mark       :: Default Bool
         }

-- instance encodeQ :: Encode (Q) where
--   encode (Q { shouldView, andOr, tagNL, prePost, mark, children }) =
--     genericEncode defaultOptions { shouldView, andOr, prePost, mark, children }
                                 -- and then do something about the tagNL map


-- it would be nice to use record wildcard constructors but i can't seem to figure it out.
-- https://github.com/purescript/documentation/blob/master/language/Records.md
-- I tried mkQ = Q <<< { shouldView: _, ... }
mkQ sv ao nl pp m c =
  Q { shouldView: sv
    , andOr:      ao -- slightly different from QoutJS, which contains children in it
    , tagNL:      nl
    , prePost:    pp
    , mark:       m
    , children:   c
    }

newtype QoutJS = QoutJS (Option.Option ( shouldView :: String
                                       , andOr      :: Option.Option ( tag :: String
                                                                     , nl :: FO.Object String
                                                                     , contents :: String
                                                                     , children :: Array QoutJS
                                                                     )
                                       , prePost    :: PrePostRecord
                                       , post       :: String
                                       , mark       :: DefaultRecord
                                       ))

derive instance  eqQoutJS :: Eq QoutJS
derive instance genericQoutJS :: Generic QoutJS _
instance showQoutJS :: Show QoutJS where show eta = genericShow eta



qoutjs :: Q -> QoutJS
qoutjs (Q q@{ shouldView, andOr, tagNL, prePost, mark, children }) =
  QoutJS $ Option.fromRecord {
    shouldView : show shouldView
    , andOr      : case andOr of And -> Option.fromRecord { tag: "All", children: qoutjs <$> children, nl: miniNL }
                                 Or  -> Option.fromRecord { tag: "Any", children: qoutjs <$> children, nl: miniNL }
                                 (Simply x) -> Option.fromRecord { tag: "Leaf"
                                                                 , contents: Just x
                                                                 , nl: miniNL }
    , prePost    : dumpPrePost prePost
    , mark       : dumpDefault mark
    }
 where miniNL =
         FO.fromFoldable (Map.toUnfoldable tagNL :: Array (Tuple String String))

newtype PrePostRecord = PPR (Option.Option ( pre :: String, post :: String ))
derive instance  eqPrePostRecord :: Eq PrePostRecord
derive instance genericPrePostRecord :: Generic PrePostRecord _
instance showPrePostRecord :: Show PrePostRecord where show eta = genericShow eta
instance encodePrePostRecord :: Encode PrePostRecord where encode eta = unsafeToForeign eta

dumpPrePost :: Maybe (Label String) -> PrePostRecord
dumpPrePost (Just (Pre x      )) = PPR $ Option.fromRecord { pre: x }
dumpPrePost (Just (PrePost x y)) = PPR $ Option.fromRecord { pre: x, post: y }
dumpPrePost (Nothing           ) = PPR $ Option.empty

maybe2string (Just x)  = show x
maybe2string Nothing   = "undefined"

data ShouldView = View | Hide | Ask
derive instance  eqShouldView ::        Eq (ShouldView)
derive instance genericShouldView :: Generic ShouldView _
instance showShouldView :: Show ShouldView where show eta = genericShow eta
instance encodeShouldView :: Encode ShouldView where encode eta = genericEncode defaultOptions eta

data AndOr a = And      -- All
             | Or       -- And
             | Simply a -- Leaf
derive instance  eqAndOr :: (Eq a) => Eq (AndOr a)
derive instance genericAndOr :: Generic (AndOr a) _
instance showAndOr :: (Show a) => Show (AndOr a) where show = genericShow
instance encodeAndOr :: (Encode a) => Encode (AndOr a) where encode eta = genericEncode defaultOptions eta

-- a few other types for configuration of the user interface

data DisplayPref = DPTerse | DPNormal | DPVerbose
derive instance  eqDisplayPref :: Eq DisplayPref
derive instance genericDisplayPref :: Generic DisplayPref _
instance showDisplayPref :: Show DisplayPref where show = genericShow
instance encodeDisplayPref :: Encode DisplayPref where encode eta = genericEncode defaultOptions eta

data Hardness = Soft -- use Left defaults
              | Hard -- require Right input
derive instance  eqHardness :: Eq (Hardness)
derive instance genericHardness :: Generic Hardness _
instance showHardness :: Show Hardness where show = genericShow
instance encodeHardness :: Encode Hardness where encode eta = genericEncode defaultOptions eta
instance decodeHardness :: Decode Hardness where decode eta = genericDecode defaultOptions eta


data StdinSchema a = StdinSchema { marking :: Marking
                                 , andOrTree :: Item a }
derive instance  eqStdinSchema :: (Eq a) => Eq (StdinSchema a)
derive instance genericStdinSchema :: Generic (StdinSchema a) _
instance showStdinSchema :: (Show a) => Show (StdinSchema a) where show = genericShow
instance encodeStdinSchema :: (Show a, Encode a) => Encode (StdinSchema a) where encode eta = genericEncode defaultOptions eta


{-
getSV :: ShouldView -> Q a -> Maybe (a, Default Bool)
getSV sv1 (Q sv2 (Simply x) pp m)
  | sv1 == sv2 = Just (x, m)
  | otherwise  = Nothing
getSV _ _ = Nothing

getAsks :: (ToJSONKey a, Ord a) => QTree a -> Map.Map a (Default Bool)
getAsks qt = Map.fromList $ catMaybes $ getSV Ask <$> flatten qt

getAsksJSON :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getAsksJSON = encode . getAsks

getViews :: (ToJSONKey a, Ord a) => QTree a -> Map.Map a (Default Bool)
getViews qt = Map.fromList $ catMaybes $ getSV View <$> flatten qt

getViewsJSON :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getViewsJSON = encode . getViews

getForUI :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getForUI qt = encode (Map.fromList [("view" :: TL.Text, getViews qt)
                                   ,("ask" :: TL.Text, getAsks qt)])

-}

newtype ForD3 = ForD3 { name :: String
                      , children :: Array ForD3
                      , value :: Int }
derive instance  eqForD3 :: Eq (ForD3)
derive instance genericForD3 :: Generic (ForD3) _
instance showForD3 :: Show (ForD3) where show eta = genericShow eta
instance encodeForD3 :: Encode ForD3 where encode eta = unsafeToForeign eta

forD3 :: String -> Q -> ForD3
forD3 lang (Q q) = ForD3 { name: qName
                         , children: forD3 lang <$> q.children
                         , value: 100 }
  where
    qName = case q.andOr of
      And        -> maybe "all of" ({- getNL <<< -} label2pre) q.prePost
      Or         -> maybe "any of" ({- getNL <<< -} label2pre) q.prePost
      (Simply x) -> shorten 45 $ getNL x
    getNL x = case Map.lookup lang q.tagNL of
        Nothing -> x
        (Just t) -> x <> ". " <> t
    shorten n x =
      if DString.length x > n
      then DString.take (n-3) x <> "..."
      else x

d3_tag = JSON.writeJSON <<< encode <<< forD3 ""
d3_en  = JSON.writeJSON <<< encode <<< forD3 "en"
