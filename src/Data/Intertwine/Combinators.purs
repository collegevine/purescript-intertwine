-- | Primitive combinators for Iso
module Data.Intertwine.Combinators
    ( isoFlip
    , isoTraverse
    , isoFrom
    , isoWrap
    , isoUnwrap
    , isoJust
    ) where

import Prelude

import Data.Intertwine.Iso (Iso(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable, sequence)

-- | Constructs a never-failing [`Iso`](#t:Iso) out of given "apply" and
-- | "inverse" functions
isoFrom :: forall a b. (a -> b) -> (b -> a) -> Iso a b
isoFrom apply inverse = Iso { apply: Just <<< apply, inverse: Just <<< inverse }

-- | Revereses the direction of an [`Iso`](#t:Iso)
isoFlip :: forall a b. Iso a b -> Iso b a
isoFlip (Iso i) = Iso { apply: i.inverse, inverse: i.apply }

-- | Given a `Traversable` and an [`Iso`](#t:Iso) that maps some values `a` and
-- | `b`, produces a new [`Iso`](#t:Iso) that maps those values wrapped in the
-- | `Traversable` - `f a` and `f b`. This is handy for working for `Maybe`, for
-- | example:
-- |
-- |     -- First, define a never-failing Iso that maps a number
-- |     -- to a number 5 greater than it
-- |     plus5 :: Iso Int Int
-- |     plus5 = isoFrom (_ + 5) (_ - 5)
-- |
-- |     > plus5.apply 37 == Just 42
-- |     > plus5.inverse 42 == Just 37
-- |
-- |     -- Now, wrap it in a `Maybe`
-- |     mPlus5 :: Iso (Maybe Int) (Maybe Int)
-- |     mPlus5 = isoTraverse plus5
-- |
-- |     > mPlus5.apply (Just 37) == Just (Just 42)
-- |     > mPlus5.inverse (Just 42) == Just (Just 37)
-- |     > mPlus5.apply Nothing == Just Nothing
-- |     > mPlus5.inverse Nothing == Just Nothing
-- |
-- |     -- Or wrap it in an `Array`
-- |     aPlus5 :: Iso (Array Int) (Array Int)
-- |     aPlus5 = isoTraverse plus5
-- |
-- |     > aPlus5.apply [37, 0] == Just [42, 5]
-- |     > aPlus5.inverse [42, 5] == Just [37, 0]
-- |     > aPlus5.apply [] == Just []
-- |     > aPlus5.inverse [] == Just []
-- |
isoTraverse :: forall f a b. Traversable f => Iso a b -> Iso (f a) (f b)
isoTraverse (Iso i) = Iso { apply: sequence <<< map i.apply, inverse: sequence <<< map i.inverse }

-- | Constructs a never-failing [`Iso`](#t:Iso) mapping a `newtype` to the type
-- | it wraps. The intended use is to provide the `newtype`'s constructor as
-- | first argument for the purpose of type inference.
-- |
-- | Example:
-- |
-- |     newtype N = N Int
-- |     derive instance newtypeN :: Newtype N _
-- |
-- |     isoN :: Iso Int N
-- |     isoN = isoWrap N
-- |
-- |     > isoN.apply 42 == Just (N 42)
-- |     > isoN.inverse (N 42) == Just 42
-- |
isoWrap :: forall w a. Newtype w a => (a -> w) -> Iso a w
isoWrap _ = isoFrom wrap unwrap

-- | The opposite of `isoWrap`: constructs a never-failing [`Iso`](#t:Iso) that
-- | maps a value to a `newtype` that wraps it. The intended use is to provide
-- | the `newtype`'s constructor as first argument for the purpose of type
-- | inference.
-- |
-- | Example:
-- |
-- |     newtype N = N Int
-- |     derive instance newtypeN :: Newtype N _
-- |
-- |     isoN :: Iso N Int
-- |     isoN = isoUnwrap N
-- |
-- |     > isoN.apply (N 42) == Just 42
-- |     > isoN.inverse 42 == Just (N 42)
-- |
isoUnwrap :: forall w a. Newtype w a => (a -> w) -> Iso w a
isoUnwrap = isoFlip <<< isoWrap

-- | An [`Iso`](#t:Iso) that wraps any value in a `Just`, and unwraps on
-- | inverse, failing when given `Nothing`.
-- |
-- | Example:
-- |
-- |     > isoJust.apply 42 == Just (Just 42)
-- |     > isoJust.inverse (Just 42) = Just 42
-- |     > isoJust.inverse Nothing = Nothing
-- |
isoJust :: forall a. Iso a (Maybe a)
isoJust = Iso { apply: Just <<< Just, inverse: identity }
