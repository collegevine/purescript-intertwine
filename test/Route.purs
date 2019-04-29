module Test.Route where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Intertwine.Route (class PathPiece, Ctor(..), PathInfo(..), RoutesDef, constValue, end, newtypeQuery, newtypeSeg, parseRoute, printRoute, query, query', seg, segValue, segValue', (*|>), (<|*|>), (<|:|>), (<|||>))
import Data.Intertwine.Syntax ((<|*))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Obj
import Test.QuickCheck (Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

data Route
    = StandardRoute StandardRoute
    | CT_Seg CustomType
    | CT_NewtypeSeg CustomType
    | CT_Query (Maybe CustomType)
    | CT_NewtypeQuery (Maybe CustomType)

data StandardRoute
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

-- | I'm pretending that I don't control this type, so I can it how it works
-- | with `segValue'`, `query'`, `newtypeSeg`, and `newtypeQuery`
data CustomType
    = CT1 String
    | CT2 Int

newtype Wrapper = Wrapper CustomType
derive instance newtypeWrapper :: Newtype Wrapper _

derive instance gRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where show = genericShow

derive instance gStandardRoute :: Generic StandardRoute _
derive instance eqStandardRoute :: Eq StandardRoute
instance showStandardRoute :: Show StandardRoute where show = genericShow
instance arbStandardRoute :: Arbitrary StandardRoute where arbitrary = genericArbitrary

derive instance gSubRoute :: Generic SubRoute _
derive instance eqSubRoute :: Eq SubRoute
instance showSubRoute :: Show SubRoute where show = genericShow
instance arbSubRoute :: Arbitrary SubRoute where arbitrary = genericArbitrary

derive instance gR2 :: Generic R2 _
derive instance eqR2 :: Eq R2
instance showR2 :: Show R2 where show = genericShow

derive instance gCustomType :: Generic CustomType _
derive instance eqCustomType :: Eq CustomType
instance showCustomType :: Show CustomType where show = genericShow

instance ppWrapper :: PathPiece Wrapper where
    toPathSegment = unwrap >>> printCustomType
    fromPathSegment = Just <<< wrap <<< CT1

route :: RoutesDef PathInfo Route
route =
          (Ctor::Ctor "StandardRoute") <|:|> standardRoute <|* end
    <|||> (Ctor::Ctor "CT_Seg") <|:|> seg "ct_seg" *|> segValue' printCustomType parsCustomType <|* end
    <|||> (Ctor::Ctor "CT_NewtypeSeg") <|:|> seg "ct_ntseg" *|> newtypeSeg Wrapper <|* end
    <|||> (Ctor::Ctor "CT_Query") <|:|> seg "ct_q" *|> query' printCustomType parsCustomType "q" <|* end
    <|||> (Ctor::Ctor "CT_NewtypeQuery") <|:|> seg "ct_ntq" *|> newtypeQuery Wrapper "q" <|* end

standardRoute :: RoutesDef PathInfo StandardRoute
standardRoute =
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

r2a :: RoutesDef PathInfo R2
r2a = (Ctor::Ctor "R2A") <|:|> query "foo" <|* end

r2b :: RoutesDef PathInfo R2
r2b = (Ctor::Ctor "R2B") <|:|> seg "bar" <|* end

parsCustomType :: String -> Maybe CustomType
parsCustomType s = (CT2 <$> Int.fromString s) <|> (Just $ CT1 s)

printCustomType :: CustomType -> String
printCustomType (CT1 s) = s
printCustomType (CT2 i) = show i

isomorphicProp :: forall r. Eq r => Show r => RoutesDef PathInfo r -> r -> Result
isomorphicProp rdef r = Just r === (printRoute rdef r >>= parseRoute rdef)

allTests :: Spec Unit
allTests = do
    describe "Quickcheck- isomorphic print/parse" do
        it "StandardRoute" $ quickCheck $ isomorphicProp standardRoute
    describe "Printing/parsing routes" do
        t route (StandardRoute Root)                    "/"
        t route (StandardRoute A)                       "/a"
        t route (StandardRoute (B "abc"))               "/b/abc"
        t route (StandardRoute (B ""))                  "/b/"
        t route (StandardRoute (C 42 $ Just 42))        "/fourty-two"
        t route (StandardRoute (C 42 $ Just 5))         "/c/d/42?second=5"
        t route (StandardRoute (C 0 $ Just 42))         "/c/d/0?second=42"
        t route (StandardRoute (C 42 Nothing))          "/c/d/42"
        t route (StandardRoute (D $ X 42))              "/d/42"
        t route (StandardRoute (D $ Y $ Just "splat"))  "/d/y?s=splat"
        t route (StandardRoute (D $ Y Nothing ))        "/d/y"

        t route (CT_Seg $ CT1 "foo")           "/ct_seg/foo"
        t route (CT_Seg $ CT2 42)              "/ct_seg/42"
        t route (CT_NewtypeSeg $ CT1 "foo")    "/ct_ntseg/foo"

        t route (CT_Query $ Just $ CT1 "foo")         "/ct_q?q=foo"
        t route (CT_Query $ Just $ CT2 42)            "/ct_q?q=42"
        t route (CT_Query Nothing)                    "/ct_q"
        t route (CT_NewtypeQuery $ Just $ CT1 "foo")  "/ct_ntq?q=foo"
        t route (CT_NewtypeQuery Nothing)             "/ct_ntq"

        let testR2 (r :: RoutesDef PathInfo R2) = do
                t r (R2A Nothing)           "/"
                t r (R2A $ Just "baz")      "/?foo=baz"
                t r R2B                     "/bar"
        testR2 $ r2a <|||> r2b
        testR2 $ r2b <|||> r2a
    where
        t :: forall r. Show r => Eq r => RoutesDef PathInfo r -> r -> String -> Spec Unit
        t syn value expectedUrl = it (show value <> " == " <> expectedUrl) do
            let printed = printRoute syn value
                parsed = parseRoute syn =<< printed
            shouldEqual (Just expectedUrl) (showPath <$> printed)
            shouldEqual (Just value) parsed

        showPath (PathInfo segs query) = "/" <> showSegs <> qMark <> showQuery
            where
                showSegs = String.joinWith "/" segs
                qMark = if showQuery == "" then "" else "?"
                showQuery = String.joinWith "&" do
                    Tuple k v <- Obj.toUnfoldable query
                    pure $ k <> "=" <> v
