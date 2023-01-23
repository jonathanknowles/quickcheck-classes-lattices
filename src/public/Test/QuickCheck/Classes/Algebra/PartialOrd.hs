{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.Algebra.PartialOrd
    (
    -- * PartialOrd
      partialOrdLaws
    )
    where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Test.QuickCheck
    ( Arbitrary (..), Property, cover )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Semigroup.Internal
    ( makeLaw1, makeLaw2, makeLaw3, makeProperty )

--------------------------------------------------------------------------------
-- PartialOrd
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'PartialOrd'.
--
-- Tests the following properties:
--
-- prop> a `leq` a
-- prop> a `leq` b && b `leq` a ==> a == b
-- prop> a `leq` b && b `leq` c ==> a `leq` c
--
partialOrdLaws
    :: forall a. (Arbitrary a, Show a, Monoid a, PartialOrd a)
    => Proxy a
    -> Laws
partialOrdLaws _ = Laws "PartialOrd"
    [ makeLaw1 @a
        "partialOrdLaw_reflexivity"
        (partialOrdLaw_reflexivity)
    , makeLaw2 @a
        "partialOrdLaw_antisymmetry"
        (partialOrdLaw_antisymmetry)
    , makeLaw3 @a
        "partialOrdLaw_transitivity"
        (partialOrdLaw_transitivity)
    ]

partialOrdLaw_reflexivity
    :: PartialOrd a => a -> Property
partialOrdLaw_reflexivity a =
    makeProperty
        "a `leq` a"
        (a `leq` a)

partialOrdLaw_antisymmetry
    :: PartialOrd a => a -> a -> Property
partialOrdLaw_antisymmetry a b =
    makeProperty
        "not (a `leq` b && b `leq` a) || a == b"
        (not (a `leq` b && b `leq` a) || a == b)
    & cover 1
        (a `leq` b && b `leq` a)
        "a `leq` b && b `leq` a"

partialOrdLaw_transitivity
    :: PartialOrd a => a -> a -> a -> Property
partialOrdLaw_transitivity x y z =
    makeProperty
        "not (a `leq` b && b `leq` c) || a `leq` c"
        (not (a `leq` b && b `leq` c) || a `leq` c)
    & cover 1
        (a `leq` b && b `leq` c)
        "a `leq` b && b `leq` c"
  where
    (a, b, c)
        | x `leq` y && y `leq` z = (x, y, z)
        | x `leq` z && z `leq` y = (x, z, y)
        | y `leq` x && x `leq` z = (y, x, z)
        | y `leq` z && z `leq` x = (y, z, x)
        | z `leq` x && x `leq` y = (z, x, y)
        | z `leq` y && y `leq` x = (z, y, x)
        | otherwise = (x, y, z)
