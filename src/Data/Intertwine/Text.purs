-- Syntax primitives for printing/parsing plain text
module Data.Intertwine.Text
    ( str
    , int
    , lit
    ) where

import Prelude

import Data.Char.Unicode as Char
import Data.Int as Int
import Data.Intertwine.Iso (Iso(..))
import Data.Intertwine.Syntax (class Syntax, atom)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..), swap)

type Primitive a = forall syn. Syntax syn => syn String a

atom' :: forall a. (a -> String) -> (String -> Maybe (Tuple a String)) -> Primitive a
atom' print parse = atom $ Iso {
    apply: \(Tuple output a) -> Just $ Tuple (output <> print a) unit,
    inverse: \(Tuple output _) -> swap <$> parse output
}

str :: Primitive String
str = atom' identity \s -> Just $ Tuple s ""

int :: Primitive Int
int = atom' show \s -> do
    let head' = String.takeWhile Char.isNumber s
    head <- if head' /= "" then Just head' else Nothing
    num <- Int.fromString head
    pure $ Tuple num $ String.drop (String.length head) s

lit :: String -> Primitive Unit
lit txt = atom $ Iso {
    apply: \(Tuple state _) -> Just $ Tuple (state <> txt) unit,
    inverse: \(Tuple state _) -> do
        tail <- String.stripPrefix (Pattern txt) state
        pure $ Tuple tail unit
}
