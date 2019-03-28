-- | Syntax primitives for printing/parsing plain text
module Data.Intertwine.Text
    ( str
    , followedBy
    , int
    , lit
    ) where

import Prelude

import Control.MonadZero (guard)
import Data.Char.Unicode as Char
import Data.Int as Int
import Data.Intertwine.Iso (Iso(..))
import Data.Intertwine.Syntax (class Syntax, atom, parse, print)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))

type Primitive a = forall syn. Syntax syn => syn String a

followedBy :: forall a. Primitive a -> String -> Primitive a
followedBy p suffix = atom $ Iso
    { apply: \(Tuple output a) -> do
        aPrinted <- print p "" a
        pure $ Tuple (output <> aPrinted <> suffix) unit

    , inverse: \(Tuple input _) -> do
        idx <- String.indexOf (Pattern suffix) input
        prefix <- String.slice 0 idx input
        Tuple a rest <- parse p prefix
        guard $ rest == ""
        let tail = String.drop (idx + String.length suffix) input
        pure $ Tuple tail a
    }

str :: Primitive String
str = atom $ Iso
    { apply: \(Tuple output s) -> Just $ Tuple (output <> s) unit
    , inverse: \(Tuple input _) -> Just $ Tuple "" input
    }

int :: Primitive Int
int = atom $ Iso
    { apply: \(Tuple output a) ->
        Just $ Tuple (output <> show a) unit
    , inverse: \(Tuple input _) -> do
        let head = String.takeWhile Char.isNumber input
        guard $ head /= ""
        num <- Int.fromString head
        let tail = String.drop (String.length head) input
        pure $ Tuple tail num
    }

lit :: String -> Primitive Unit
lit txt = atom $ Iso
    { apply: \(Tuple state _) ->
        Just $ Tuple (state <> txt) unit
    , inverse: \(Tuple state _) -> do
        tail <- String.stripPrefix (Pattern txt) state
        pure $ Tuple tail unit
    }
