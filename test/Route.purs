module Test.Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Intertwine.Route (Ctor(..), PathInfo(..), RoutesDef, end, constValue, seg, parseRoute, printRoute, query, segValue, (*|>), (<|*|>), (<|:|>), (<|||>))
import Data.Intertwine.Syntax ((<|*))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Obj
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

data R2
    = R2A (Maybe String)
    | R2B

derive instance gRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where show = genericShow

derive instance gSubRoute :: Generic SubRoute _
derive instance eqSubRoute :: Eq SubRoute
instance showSubRoute :: Show SubRoute where show = genericShow

derive instance gR2 :: Generic R2 _
derive instance eqR2 :: Eq R2
instance showR2 :: Show R2 where show = genericShow

route :: RoutesDef PathInfo Route
route =
          (Ctor::Ctor "Root") <|:|> end
    <|||> (Ctor::Ctor "A") <|:|> seg "a" <|* end
    <|||> (Ctor::Ctor "B") <|:|> seg "b" *|> segValue <|* end
    <|||> (Ctor::Ctor "C") <|:|> seg "fourty-two" *|> constValue 42 <|*|> constValue (Just 42) <|* end
    <|||> (Ctor::Ctor "C") <|:|> seg "c" *|> seg "d" *|> segValue <|*|> query "second" <|* end
    <|||> (Ctor::Ctor "D") <|:|> seg "d" *|> subRoute <|* end

subRoute :: RoutesDef PathInfo SubRoute
subRoute =
          (Ctor::Ctor "X") <|:|> segValue
    <|||> (Ctor::Ctor "Y") <|:|> seg "y" *|> query "s"

r2a :: RoutesDef PathInfo R2
r2a = (Ctor::Ctor "R2A") <|:|> query "foo" <|* end

r2b :: RoutesDef PathInfo R2
r2b = (Ctor::Ctor "R2B") <|:|> seg "bar" <|* end

allTests :: TestSuite
allTests = suite "Printing/parsing routes" do
    t route Root                    "/"
    t route A                       "/a"
    t route (B "abc")               "/b/abc"
    t route (B "")                  "/b/"
    t route (C 42 $ Just 42)        "/fourty-two"
    t route (C 42 $ Just 5)         "/c/d/42?second=5"
    t route (C 0 $ Just 42)         "/c/d/0?second=42"
    t route (C 42 Nothing)          "/c/d/42"
    t route (D $ X 42)              "/d/42"
    t route (D $ Y $ Just "splat")  "/d/y?s=splat"
    t route (D $ Y Nothing )        "/d/y"

    let testR2 (r :: RoutesDef PathInfo R2) = do
            t r (R2A Nothing)           "/"
            t r (R2A $ Just "baz")      "/?foo=baz"
            t r R2B                     "/bar"
    testR2 $ r2a <|||> r2b
    testR2 $ r2b <|||> r2a
    where
        t :: forall r. Show r => Eq r => RoutesDef PathInfo r -> r -> String -> TestSuite
        t syn value expectedUrl = test (show value <> " == " <> expectedUrl) do
            let printed = printRoute syn value
                parsed = parseRoute syn =<< printed
            equal (Just expectedUrl) (showPath <$> printed)
            equal (Just value) parsed

        showPath (PathInfo segs query) = "/" <> showSegs <> qMark <> showQuery
            where
                showSegs = String.joinWith "/" segs
                qMark = if showQuery == "" then "" else "?"
                showQuery = String.joinWith "&" do
                    Tuple k v <- Obj.toUnfoldable query
                    pure $ k <> "=" <> v
