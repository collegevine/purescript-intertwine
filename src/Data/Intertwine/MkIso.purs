module Data.Intertwine.MkIso(
    class MkIso, iso,
    class ArgsAsTuple, argsToTuple, tupleToArgs
) where

import Prelude
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Intertwine.Iso (Iso(..))

-- | This type class provides the function `iso`, which operates on a sum type.
-- | It takes the name of a constructor of that type and returns an `Iso`
-- | instance that converts between the sum type itself and the chosen
-- | constructor's parameters that are all tupled together, starting with the
-- | right ones.
-- |
-- | For example:
-- |
-- |     data T = A | B String | C Int Number | D Boolean Int String
-- |
-- |     iso (SProxy :: SProxy "A") :: Iso Unit T
-- |     iso (SProxy :: SProxy "B") :: Iso String T
-- |     iso (SProxy :: SProxy "C") :: Iso (Tuple Int Number) T
-- |     iso (SProxy :: SProxy "D") :: Iso (Tuple Boolean (Tuple Int String)) T
-- |
-- | Such tupling is necessary for the implementation of both printers and
-- | parsers from the same code structure. See
-- | [`Syntax`](#t:Data.Intertwine.Syntax) for a more detailed explanation.
-- |
-- | The resulting [`Iso`](#t:Iso) can always convert "forward" (i.e. from
-- | tupled arguments to `T`), but it can only convert "backward" (i.e. from T
-- | to the corresponding tuple) when the given `T` value was constructed with
-- | the given constructor, returning `Nothing` for all other constructors.

--
-- Class parameters:
--
--      * t     - the sum type for which to generate the Iso
--      * ctor  - name of the constructor for which to generate the Iso
--      * tuple - type of the resulting tuple
--
-- The implementation has two stages: first we implement the class for the
-- Generic representation types, matching on `Constructor` and `Sum`, and then
-- we use that as a base to implement the class for the sum type itself, using
-- `to` and `from` to convert to/from the generic representation.
--
-- NOTE: the implementation relies on the fact that the compiler generates
-- `Sum` instances as a chain, even though they could technically form a tree.
-- That is, we only consider the case when the first argument of `Sum` is a
-- `Constructor`, disregarding the possibility of it being another `Sum`. As
-- far as I can tell, the compiler always generates generic rep types this
-- way.
class MkIso t ctor tuple | t ctor -> tuple where
    iso :: SProxy ctor -> Iso tuple t

-- Iso for a single constructor
instance mkIsoCtor :: ArgsAsTuple args argsAsTuple => MkIso (Constructor name args) name argsAsTuple where
    iso _ = Iso
        { apply: Just <<< Constructor <<< tupleToArgs
        , inverse: \(Constructor args) -> Just $ argsToTuple args
        }

-- Iso for a constructor that has a chain of other constructors attached.
else instance mkIsoSumLeft :: ArgsAsTuple args argsAsTuple => MkIso (Sum (Constructor ctor args) rest) ctor argsAsTuple where
    iso _ = Iso
        { apply:
            Just <<< Inl <<< Constructor <<< tupleToArgs
        , inverse: \s -> case s of
            Inl (Constructor args) -> Just $ argsToTuple args
            Inr _ -> Nothing
        }

-- Iso for a case when the first constructor in the chain doesn't match the
-- given constructor name. In this case, we just delegate to the instance of
-- this class for the rest of the chian (if such instance exists).
else instance mkIsoSumRight :: MkIso rest ctor tuple => MkIso (Sum (Constructor anotherName y) rest) ctor tuple where
    iso _ = Iso
        { apply:
            map Inr <<< restIso.apply
        , inverse: \s -> case s of
            Inl _ -> Nothing
            Inr rest -> restIso.inverse rest
        }
        where
            Iso restIso = iso (SProxy :: SProxy ctor)

-- The top-level instance: converts from the sum type to generic rep and then
-- delegates to one of the instances above.
else instance mkIso :: (Generic t rep, MkIso rep ctor tuple) => MkIso t ctor tuple where
    iso _ = Iso
        { apply: map to <<< repIso.apply
        , inverse: repIso.inverse <<< from
        }
        where
            Iso repIso = iso (SProxy :: SProxy ctor)


-- | This type class takes the Generic-rep representation of sum type arguments
-- | and converts them into a series of nested tuples.
--
-- NOTE: similarly to the `MkIso` implementation, we rely here on the fact that
-- the first argument of `Product` is always an `Argument`, even though
-- technically it could be another `Product`. As far as I can tell, the compiler
-- always generates representations this way.
class ArgsAsTuple args tuple | args -> tuple where
    argsToTuple :: args -> tuple
    tupleToArgs :: tuple -> args

instance a2tEmpty :: ArgsAsTuple NoArguments Unit where
    argsToTuple = const unit
    tupleToArgs = const NoArguments

instance a2tSingle :: ArgsAsTuple (Argument a) a where
    argsToTuple (Argument a) = a
    tupleToArgs = Argument

instance a2tRecursive :: (ArgsAsTuple a ax, ArgsAsTuple b bx) => ArgsAsTuple (Product a b) (Tuple ax bx) where
    argsToTuple (Product a b) = Tuple (argsToTuple a) (argsToTuple b)
    tupleToArgs (Tuple a b) = Product (tupleToArgs a) (tupleToArgs b)
