-- Syntax primitives and convenience wrappers for printing/parsing in-browser
-- routes
module Data.Intertwine.Route
    ( class IsRoute, routeEmpty, routeSegments, routeQueryString
    , PathInfo(..)
    , RoutesDef
    , parseRoute
    , printRoute

    , end
    , seg
    , segValue
    , constValue
    , query

    , module SyntaxReexport
    , module Data.Intertwine.Route.PathPiece
    ) where

import Prelude

import Control.MonadZero (guard, (<|>))
import Data.Array as Array
import Data.Intertwine.Iso (Iso(..))
import Data.Intertwine.Route.PathPiece (class PathPiece, toPathSegment, fromPathSegment)
import Data.Intertwine.Syntax (Ctor(..), (<|$|>), (<|:|>), (<|*|>), (*|>), (<|||>)) as SyntaxReexport
import Data.Intertwine.Syntax (class Syntax, atom, parse, print)
import Data.Lens (Lens', lens, (^.), (%~), (.~))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Obj

-- | This class abstracts the idea of the "route" data type, making it possible
-- | for the primitive in this module to work with data types from other
-- | libraries.
class IsRoute r where
    routeEmpty :: r
    routeSegments :: Lens' r (Array String)
    routeQueryString :: Lens' r (Obj.Object String)


-- | The default representation of a route: here the route is represented as a
-- | sequence of path segments and a dictionary of querystring parameters.
data PathInfo = PathInfo (Array String) (Obj.Object String)

instance pathIsRoute :: IsRoute PathInfo where
    routeEmpty = PathInfo [] Obj.empty
    routeSegments = lens (\(PathInfo s _) -> s) (\(PathInfo _ q) s -> PathInfo s q)
    routeQueryString = lens (\(PathInfo _ q) -> q) (\(PathInfo s _) q -> PathInfo s q)


-- | Syntax definition for a set of routes of type `a`.
type RoutesDef route a = forall syntax. Syntax syntax => syntax route a

parseRoute :: forall a route. IsRoute route => RoutesDef route a -> route -> Maybe a
parseRoute def path = do
    Tuple a rt <- parse def path
    guard $ Array.null $ rt^.routeSegments
    pure a

printRoute :: forall a route. IsRoute route => RoutesDef route a -> a -> Maybe route
printRoute def = print def routeEmpty

-- | Signifies the end of the route. During printing doesn't produce any output,
-- | during parsing makes sure that there are no URL segments remaining.
end :: forall route. IsRoute route => RoutesDef route Unit
end = mkAtom prnt pars
    where
        prnt pi _ = Just pi
        pars r | Array.null (r^.routeSegments) = Just $ Tuple r unit
        pars _ = Nothing

-- | Path segment that is a literal string. During printing outputs the given
-- | string, during parsing consumes the next URL segment and makes sure it's
-- | equal to the given string.
seg :: forall route. IsRoute route => String -> RoutesDef route Unit
seg str = mkAtom prnt pars
    where
        prnt pi _ =
            Just $ appendSeg str pi
        pars r = do
            l <- Array.uncons (r^.routeSegments)
            guard $ l.head == str
            pure $ Tuple (r # routeSegments .~ l.tail) unit

-- | A primitive that encodes a constant value. During printing, the printer
-- | succeeds iff the value beign printed is equal to `theValue`, otherwise
-- | fails. During parsing, the parser returns `theValue` without consuming any
-- | input.
constValue :: forall a route. Eq a => a -> RoutesDef route a
constValue theValue = mkAtom prnt pars
    where
        prnt pi a | a == theValue = Just pi
        prnt _ _ = Nothing
        pars pi = Just $ Tuple pi theValue

-- | A value of the given type as URL segment. During printing, the printer
-- | outputs the value as a URL segment, using the `PathPiece` instance to
-- | convert it to a string. During parsing, the parser consumes a URL segment
-- | and tries to parse it into a value of the given type using the `PathPiece`
-- | instance.
segValue :: forall a route. IsRoute route => PathPiece a => RoutesDef route a
segValue = mkAtom prnt pars
    where
        prnt pi a =
            Just $ appendSeg (toPathSegment a) pi
        pars r = do
            l <- Array.uncons (r^.routeSegments)
            a <- fromPathSegment l.head
            pure $ Tuple (r # routeSegments .~ l.tail) a

-- | QueryString value. During printing adds the printed value to the
-- | QueryString under given key. During parsing, looks up the value in the
-- | QueryString.
query :: forall a route. IsRoute route => PathPiece a => String -> RoutesDef route (Maybe a)
query key = mkAtom prnt \pi -> pars pi <|> fallback pi
    where
        prnt r Nothing =
            Just r
        prnt r (Just a) =
            Just $ r # routeQueryString %~ Obj.insert key (toPathSegment a)

        pars r = do
            v <- Obj.lookup key $ r^.routeQueryString
            a <- fromPathSegment v
            pure $ Tuple (r # routeQueryString %~ Obj.delete key) (Just a)

        fallback r =
            Just $ Tuple r Nothing



--
-- Internal utilities
--

appendSeg :: forall route. IsRoute route => String -> route -> route
appendSeg seg r = r # routeSegments %~ (_ `Array.snoc` seg)

-- | Helper function for producing an Iso out of a print function and a parse
-- | function. It's here solely to shorten the code of primitives above by
-- | removing some of the `Tuple` cruft from them.
mkAtom :: forall a route
     . (route -> a -> Maybe route)        -- ^ Printing function
    -> (route -> Maybe (Tuple route a))   -- ^ Parsing function
    -> RoutesDef route a
mkAtom printA parseA = atom $ Iso {
    apply: \(Tuple route a) -> printA route a <#> \newRoute -> Tuple newRoute unit,
    inverse: \(Tuple route _) -> parseA route
}
