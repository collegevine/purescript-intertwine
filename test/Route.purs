module Test.Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Intertwine.Route (R(..), PathInfo(..), RoutesDef, empty, exactly, literal, parseRoute, printRoute, query, value, (*|>), (<|$|>), (<|*|>), (<|||>))
import Data.Maybe (Maybe(..), fromJust)
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Obj
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

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

derive instance gSubRoute :: Generic SubRoute _
derive instance eqSubRoute :: Eq SubRoute
instance showSubRoute :: Show SubRoute where show = genericShow

route :: RoutesDef PathInfo Route
route =
          (R::R "Root") <|$|> empty
    <|||> (R::R "A") <|$|> literal "a"
    <|||> (R::R "B") <|$|> literal "b" *|> value
    <|||> (R::R "C") <|$|> literal "fourty-two" *|> exactly 42 <|*|> exactly (Just 42)
    <|||> (R::R "C") <|$|> literal "c" *|> literal "d" *|> value <|*|> query "second"
    <|||> (R::R "D") <|$|> literal "d" *|> subRoute

subRoute :: RoutesDef PathInfo SubRoute
subRoute =
          (R::R "X") <|$|> value
    <|||> (R::R "Y") <|$|> literal "y" *|> query "s"

allTests :: TestSuite
allTests = suite "Printing/parsing routes" do
    t Root                    "/"
    t A                       "/a"
    t (B "abc")               "/b/abc"
    t (B "")                  "/b/"
    t (C 42 $ Just 42)        "/fourty-two"
    t (C 42 $ Just 5)         "/c/d/42?second=5"
    t (C 0 $ Just 42)         "/c/d/0?second=42"
    t (C 42 Nothing)          "/c/d/42"
    t (D $ X 42)              "/d/42"
    t (D $ Y $ Just "splat")  "/d/y?s=splat"
    t (D $ Y Nothing )        "/d/y"
    where
        t value expectedUrl = unsafePartial $ test (show value <> " == " <> expectedUrl) do
            let printed = fromJust $ printRoute route value
                parsed = fromJust $ parseRoute route printed
            equal expectedUrl (showPath printed)
            equal value parsed

        showPath (PathInfo segs query) = "/" <> showSegs <> qMark <> showQuery
            where
                showSegs = String.joinWith "/" segs
                qMark = if showQuery == "" then "" else "?"
                showQuery = String.joinWith "&" do
                    Tuple k v <- Obj.toUnfoldable query
                    pure $ k <> "=" <> v
