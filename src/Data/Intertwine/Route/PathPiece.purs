module Data.Intertwine.Route.PathPiece
    ( class PathPiece
    , toPathSegment
    , fromPathSegment
    ) where

import Prelude
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.UUID as UUID

class PathPiece a where
    toPathSegment :: a -> String
    fromPathSegment :: String -> Maybe a

instance pathPieceString :: PathPiece String where
    toPathSegment = identity
    fromPathSegment = Just

instance pathPieceInt :: PathPiece Int where
    toPathSegment = show
    fromPathSegment = Int.fromString

instance pathPieceUUID :: PathPiece UUID.UUID where
    toPathSegment = show
    fromPathSegment = UUID.parseUUID
