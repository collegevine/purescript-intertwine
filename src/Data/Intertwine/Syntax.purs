module Data.Intertwine.Syntax
    ( class Syntax, atom, synApply, synInject, alt, (<|*|>), (<|$|>), (<|||>)
    , dropUnit, (*|>)
    , Printer, print
    , Parser, parse
    ) where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd, swap)
import Data.Intertwine.Iso (Iso(..))

-- An implementation of reversible printer-parser, based on this paper:
-- http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf
--
-- The goal is to provide the means for expressing representation of data
-- structures in a way that allows the same representation to be used for both
-- parsing and printing, thus eliminating the need for code duplication and
-- ensuring that the printer and the parser don't diverge.
--
-- The type variable `syntax` here represents either a printer or a parser (see
-- instances below). This printer-or-parser type, in turn, takes two generic
-- parameters: parsing/printing state and the value being parsed/printed.
--
-- The thought process goes like this: first, we realize that all data is
-- representable as sum types, so we limit ourselves to working with sum types,
-- or at least with something functionally equivalent.
--
-- Next, since our syntax has to convert the value both ways, it follows that we
-- need, for every constructor of the sum type, a way to convert from its
-- parameters to the type and back again. This concept is represented by the
-- partial isomorphisms defined in ./Iso.purs (partial, because the conversion
-- is not always possible). These isomorphisms are then automatically generated
-- for each constructor via `Generic` - this generation code is in ./MkIso.purs
--
-- Once we have an isomorphism for a constructor, it is tempting to follow the
-- familiar parser structure:
--
--      data Foo = Foo Int String
--      parseFoo = Foo <$> parseInt <*> parseString
--
-- However, the usual Functor style won't work here, because our syntax needs
-- not only to produce values (which is what Functors handle), but also to
-- _consume_ them.
--
-- In order to do that, we work the other way around - instead of gradually
-- accumulating the partially applied function within the functor from left to
-- right, we work from from right to left, first accumulating all the
-- parsers/printers that need to be "applied" to the constructor, and only then
-- injecting the constructor itself.
--
-- Thus:
--
--     pa :: Printer a
--     pb :: Printer b
--     pa `synApply` pb :: Printer (a, b)
--
-- Combining the two printers gives us a printer that can print a tuple (same
-- for parsers). After that, such printer can be applied to the
-- constructor-derived Iso:
--
--     p :: Printer (a, b)
--     i :: Iso T (a, b)
--     i `synInject` p :: Printer T
--
-- To accomodate this, we generate the Iso instances for every constructor in
-- such a way that converts the constructor's parameters into tuples, e.g.:
--
--     data T = A Int String | B Int String Boolean
--     iso "A"  :: Iso T (Int, String)
--     iso "B"  :: Iso T (Int, (String, Boolean))
--
-- This structure nicely matches the structure that results from repeatedly
-- `synApply`ing printers/parsers, provided both `synApply` and `synInject` are
-- right-associative:
--
--     pInt :: Printer Int
--     pString :: Printer String
--     pBoolean :: Printer Boolean
--     (iso "B") `synInject` pInt `synApply` pString `synApply` pBoolean
--
-- The `alt` operation allows to combine multiple printers/parsers and try them
-- out in order - very similar to the (<|>) operator from `Control.Alt`.
--
-- Finally, the `atom` operation can be used for creating primitive parsers by
-- providing an `Iso (state, a) (state, ())`. The meaning of such `Iso` is the
-- following. The signature of a printing function is `state -> a -> Maybe
-- state`, taken to mean that a printing function takes the state accumulated so
-- far (e.g. a string that has been printed so far), then takes a value to be
-- printed, and returns new state. The `Maybe` indicates that printing may fail.
-- Conversely, the signature of a parsing function is `state -> Maybe (a,
-- state)`, taken to mean that the function takes the state (e.g. the tail of
-- the input string that hasn't yet been consumed) and returns both the parsed
-- value and the new state (e.g. the new string tail, after consuming whatever
-- was needed to parse the value). To recap:
--
--       print :: state -> a -> Maybe state
--       parse :: state -> Maybe (a, state)
--
-- Applying isomorphic manipulations to the signature - specifically, uncurrying
-- the arguments, commuting tuples, and noting that `x` is isomorphic to
-- `(x,())` - we can get:
--
--       print :: (state, a) -> Maybe (state, ())
--       parse :: (state, ()) -> Maybe (state, a)
--
-- Which is equivalent to `Iso (state, a) (state, ())`. Thus, we can treat an
-- `Iso` as a pair of print+parse functions, and thus we can use it to construct
-- a primitive parser/printer.
class Syntax syntax where
    atom :: forall a state. Iso (Tuple state a) (Tuple state Unit) -> syntax state a
    synApply :: forall a b state. syntax state a -> syntax state b -> syntax state (Tuple a b)
    synInject :: forall a b state. Iso a b -> syntax state b -> syntax state a
    alt :: forall a state. syntax state a -> syntax state a -> syntax state a


infixr 5 synApply as <|*|>
infixr 5 synInject as <|$|>
infixl 2 alt as <|||>

infixr 5 dropUnit as *|>

-- Combines two printers/parsers similarly to `synApply`, but ignoring the left
-- printer/parser, provided it returns/consumes a unit.
dropUnit :: forall a syntax state. Syntax syntax => syntax state Unit -> syntax state a -> syntax state a
dropUnit u ab = i <|$|> u <|*|> ab
    where i = Iso { apply: Just <<< Tuple unit, inverse: Just <<< snd }


--
-- Printer
--

newtype Printer state a = Printer (Tuple state a -> Maybe state)

instance printerSyntax :: Syntax Printer where
    atom (Iso i) = Printer $ map fst <<< i.apply
    synApply (Printer pa) (Printer pb) = Printer \(Tuple state (Tuple a b)) -> do
        s' <- pa $ Tuple state a
        pb $ Tuple s' b
    synInject (Iso i) (Printer pb) = Printer \(Tuple state a) -> do
        b <- i.apply a
        pb $ Tuple state b
    alt (Printer p1) (Printer p2) = Printer \x -> p1 x <|> p2 x

print :: forall state a. Printer state a -> state -> a -> Maybe state
print (Printer p) state a = p $ Tuple state a


--
-- Parser
--

newtype Parser state a = Parser (state -> Maybe (Tuple a state))

instance parserSyntax :: Syntax Parser where
    atom (Iso i) = Parser \state -> swap <$> i.inverse (Tuple state unit)
    synApply (Parser pa) (Parser pb) = Parser \s -> do
        Tuple a s' <- pa s
        Tuple b s'' <- pb s'
        pure $ Tuple (Tuple a b) s''
    synInject (Iso i) (Parser pb) = Parser \s -> do
        Tuple b s' <- pb s
        a <- i.inverse b
        pure $ Tuple a s'
    alt (Parser p1) (Parser p2) = Parser \s -> p1 s <|> p2 s

parse :: forall state a. Parser state a -> state -> Maybe (Tuple a state)
parse (Parser p) s = p s
