module Test.RouteQuickCheck where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Intertwine.Route (Ctor(..), PathInfo, RoutesDef, constValue, end, parseRoute, printRoute, query, seg, segValue, (*|>), (<|*|>), (<|:|>), (<|||>))
import Data.Intertwine.Syntax ((<|*))
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Route
    = Root
    | A
    | B String
    | C Int (Maybe Int)
    | D SubRoute

data SubRoute
    = X Int
    | Y (Maybe String)

derive instance gRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where show = genericShow
instance arbRoute :: Arbitrary Route where arbitrary = genericArbitrary

derive instance gSubRoute :: Generic SubRoute _
derive instance eqSubRoute :: Eq SubRoute
instance showSubRoute :: Show SubRoute where show = genericShow
instance arbSubRoute :: Arbitrary SubRoute where arbitrary = genericArbitrary

route :: RoutesDef PathInfo Route
route =
          (Ctor::Ctor "Root") <|:|> end
    <|||> (Ctor::Ctor "A") <|:|> seg "a" <|* end
    <|||> (Ctor::Ctor "B") <|:|> seg "b" *|> segValue <|* end
    <|||> (Ctor::Ctor "C") <|:|> seg "fourty-two" *|> constValue 42 <|*|> constValue (Just 42) <|* end
    <|||> (Ctor::Ctor "C") <|:|> seg "c" *|> seg "d" *|> segValue <|*|> query "second" <|* end
    <|||> (Ctor::Ctor "C") <|:|> seg "c" *|> seg "d" *|> segValue <|*|> query "second" <|* end
    <|||> (Ctor::Ctor "D") <|:|> seg "d" *|> subRoute <|* end

subRoute :: RoutesDef PathInfo SubRoute
subRoute =
          (Ctor::Ctor "X") <|:|> segValue
    <|||> (Ctor::Ctor "Y") <|:|> seg "y" *|> query "s"

isomorphicProp :: Route -> Result
isomorphicProp r = Just r === (printRoute route r >>= parseRoute route)

allTests :: TestSuite
allTests = suite "Quickcheck" do
    test "isomorphic print/parse" $ quickCheck isomorphicProp
