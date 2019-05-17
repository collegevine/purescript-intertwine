module Test.Combinators where

import Prelude

import Data.Either (Either(..))
import Data.Intertwine.Combinators (isoFlip, isoFrom, isoJust, isoTraverse, isoUnwrap, isoWrap)
import Data.Intertwine.Iso (Iso(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

allTests :: Spec Unit
allTests = describe "Combinators" do
    describe "isoTraverse" do
        it "traverses Array" do
            shouldEqual (ap arrPlus5 [0, 1, 42]) (Just [5, 6, 47])
            shouldEqual (inv arrPlus5 [5, 6, 47]) (Just [0, 1, 42])
        it "traverses Either" do
            shouldEqual (ap eitherPlus5 $ Right 42) (Just $ Right 47)
            shouldEqual (ap eitherPlus5 $ Left "foo") (Just $ Left "foo")
            shouldEqual (inv eitherPlus5 $ Right 42) (Just $ Right 37)
            shouldEqual (inv eitherPlus5 $ Left "foo") (Just $ Left "foo")
    describe "isoFlip" do
        it "inverses direction" do
            shouldEqual (ap (isoFlip plus5) 42) (Just 37)
            shouldEqual (inv (isoFlip plus5) 42) (Just 47)
    describe "isoWrap" do
        it "wraps newtype constructor" do
            shouldEqual (ap (isoWrap N) 42) (Just (N 42))
            shouldEqual (inv (isoWrap N) (N 42)) (Just 42)
    describe "isoUnrap" do
        it "unwraps newtype constructor" do
            shouldEqual (ap (isoUnwrap N) (N 42)) (Just 42)
            shouldEqual (inv (isoUnwrap N) 42) (Just (N 42))
    describe "isoJust" do
        it "wraps in Just" do
            shouldEqual (ap isoJust 42) (Just (Just 42))
            shouldEqual (inv isoJust (Just 42)) (Just 42)
            shouldEqual (inv isoJust Nothing) (Nothing :: Maybe Int)
    where
        ap :: forall a b. Iso a b -> a -> Maybe b
        ap (Iso i) = i.apply

        inv :: forall a b. Iso a b -> b -> Maybe a
        inv (Iso i) = i.inverse

newtype N a = N a
derive instance newtypeN :: Newtype (N a) _
derive newtype instance showN :: Show a => Show (N a)
derive newtype instance eqN :: Eq a => Eq (N a)

plus5 :: Iso Int Int
plus5 = isoFrom (_ + 5) (_ - 5)

arrPlus5 :: Iso (Array Int) (Array Int)
arrPlus5 = isoTraverse plus5

eitherPlus5 :: Iso (Either String Int) (Either String Int)
eitherPlus5 = isoTraverse plus5
