module Data.Intertwine.Iso(
    Iso(..)
) where

import Prelude
import Data.Maybe (Maybe(..))

-- | Partial isomorphism - a pair of functions that can convert between two
-- | types, with a possibility of failure.
data Iso a b = Iso {
    apply :: a -> Maybe b,
    inverse :: b -> Maybe a
}


instance cIso :: Category Iso where
    identity = Iso { apply: Just, inverse: Just }

instance smgIso :: Semigroupoid Iso where
    compose (Iso a) (Iso b) = Iso {
        apply: a.apply <=< b.apply,
        inverse: a.inverse >=> b.inverse
    }
