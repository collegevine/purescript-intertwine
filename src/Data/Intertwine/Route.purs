-- Syntax primitives and convenience wrappers for printing/parsing in-browser
-- routes
module Data.Intertwine.Route
    ( class IsRoute, routeEmpty, routeSegments, routeQueryString
    , class PathPiece, toPathSegment, fromPathSegment
    , parseRoute
    , printRoute
    , empty
    , literal
    , value
    , exactly
    , query
    , R(..)
    , injectConstructor
    , (<<$>>)
    , module SyntaxReexport
    , RoutesDef
) where

import Prelude

import Control.MonadZero (guard, (<|>))
import Data.Array as Array
import Data.Intertwine.Iso (Iso(..))
import Data.Intertwine.MkIso (class MkIso, iso)
import Data.Intertwine.Syntax ((<|*|>), (*|>), (<|||>)) as SyntaxReexport
import Data.Intertwine.Syntax (class Syntax, atom, synInject, print, parse)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Obj
import Optic.Getter ((^.))
import Optic.Setter ((%~), (.~))
import Optic.Types (Lens')

class IsRoute r where
    routeEmpty :: r
    routeSegments :: Lens' r (Array String)
    routeQueryString :: Lens' r (Obj.Object String)

class PathPiece a where
    toPathSegment :: a -> String
    fromPathSegment :: String -> Maybe a

-- Syntax definition for a set of routes of type `a`.
type RoutesDef route a = forall syntax. Syntax syntax => syntax route a

parseRoute :: forall a route. IsRoute route => RoutesDef route a -> route -> Maybe a
parseRoute def path = do
    Tuple a rt <- parse def path
    guard $ Array.null $ rt^.routeSegments
    pure a

printRoute :: forall a route. IsRoute route => RoutesDef route a -> a -> Maybe route
printRoute def = print def routeEmpty

-- Empty route. During printing doesn't produce any output, during parsing makes
-- sure that there are no URL segments remaining.
empty :: forall route. IsRoute route => RoutesDef route Unit
empty = mkAtom prnt pars
    where
        prnt pi _ = Just pi
        pars r | Array.null (r^.routeSegments) = Just $ Tuple r unit
        pars _ = Nothing

-- Literal string. During printing outputs the given string, during parsing
-- consumes the next URL segment and makes sure it's equal to the given string.
literal :: forall route. IsRoute route => String -> RoutesDef route Unit
literal str = mkAtom prnt pars
    where
        prnt pi _ =
            Just $ appendSeg str pi
        pars r = do
            l <- Array.uncons (r^.routeSegments)
            guard $ l.head == str
            pure $ Tuple (r # routeSegments .~ l.tail) unit

-- A primitive that encodes a constant value. During printing, the printer
-- succeeds iff the value beign printed is equal to `theValue`, otherwise fails.
-- During parsing, the parser returns `theValue` without consuming any input.
exactly :: forall a route. Eq a => a -> RoutesDef route a
exactly theValue = mkAtom prnt pars
    where
        prnt pi a | a == theValue = Just pi
        prnt _ _ = Nothing
        pars pi = Just $ Tuple pi theValue

-- A value of the given type as URL segment. During printing, the printer
-- outputs the value as a URL segment, using the `PathPiece` instance to convert
-- it to a string. During parsing, the parser consumes a URL segment and tries
-- to parse it into a value of the given type using the `PathPiece` instance.
value :: forall a route. IsRoute route => PathPiece a => RoutesDef route a
value = mkAtom prnt pars
    where
        prnt pi a =
            Just $ appendSeg (toPathSegment a) pi
        pars r = do
            l <- Array.uncons (r^.routeSegments)
            a <- fromPathSegment l.head
            pure $ Tuple (r # routeSegments .~ l.tail) a

-- QueryString value. During printing adds the printed value to the QueryString
-- under given key. During parsing, looks up the value in the QueryString.
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


-- This type is equivalent to `SProxy`, but provided here separately for the
-- purpose of shortening the code. See comments on `injectConstructor`.
data R (name :: Symbol) = R

infixr 5 injectConstructor as <<$>>

-- Meant to be used as infix operator, binds a constructor, whose name is
-- encoded in the `R` value, to the given parser/printer.
--
-- For example:
--
--    data T = A String | B (Maybe Int)
--
--    syntax =
--            (R::R "A") <<$>> value
--      <<|>> (R::R "B") <<$>> query "id"
--
injectConstructor :: forall name args a syntax route. MkIso a name args => Syntax syntax => R name -> syntax route args -> syntax route a
injectConstructor _ args = synInject (iso (SProxy :: SProxy name)) args


--
-- Internal utilities
--

appendSeg :: forall route. IsRoute route => String -> route -> route
appendSeg seg r = r # routeSegments %~ (_ `Array.snoc` seg)

-- Helper function for producing an Iso out of a print function and a parse
-- function. It's here solely to shorten the code of primitives above by
-- removing some of the `Tuple` cruft from them.
mkAtom :: forall a route
     . (route -> a -> Maybe route)        -- ^ Printing function
    -> (route -> Maybe (Tuple route a))   -- ^ Parsing function
    -> RoutesDef route a
mkAtom printA parseA = atom $ Iso {
    apply: \(Tuple route a) -> printA route a <#> \newRoute -> Tuple newRoute unit,
    inverse: \(Tuple route _) -> parseA route
}
