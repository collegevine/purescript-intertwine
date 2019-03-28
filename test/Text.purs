--
-- This test suite is a proof of concept, demonstrating how the Syntax machinery
-- can be used to implement reversible printers/parsers for a simple case where
-- the input of parsing (aka output of printing) is a text string.
--
-- The test itself enumerates a few concrete examples, and for each one makes
-- sure that `printer >>> parser == identity`. A better approach would be to
-- employ QuickCheck to prove that this property holds for any input. This is
-- TODO.
--
module Test.Text where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Intertwine.MkIso (iso)
import Data.Intertwine.Syntax (class Syntax, parse, print, (*|>), (<|$|>), (<|*|>), (<|||>))
import Data.Intertwine.Text (int, lit, str, followedBy)
import Data.Maybe (fromJust)
import Data.Symbol (SProxy(..))
import Data.Tuple (fst)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

data T
    = A
    | B String
    | C Int Int

derive instance gT :: Generic T _
derive instance eqT :: Eq T
instance showT :: Show T where show = genericShow

p :: forall syn. Syntax syn => syn String T
p =
          iso (SProxy :: SProxy "A") <|$|> lit "A:"
    <|||> iso (SProxy :: SProxy "B") <|$|> lit "B::" *|> (str `followedBy` "::")
    <|||> iso (SProxy :: SProxy "C") <|$|> lit "C--" *|> int <|*|> lit "/" *|> int

allTests :: TestSuite
allTests = suite "Syntax for parsing/printing strings" do
    t A "A:"
    t (B "abc") "B::abc::"
    t (B "") "B::::"
    t (C 42 42) "C--42/42"
    t (C 42 5) "C--42/5"
    t (C 0 42) "C--0/42"
    where
        t value expected = unsafePartial do
            let printed = fromJust $ print p "" value
                parsed = fst $ fromJust $ parse p printed
            test (show value <> " <==> " <> expected) $ do
                equal value parsed
                equal printed expected


