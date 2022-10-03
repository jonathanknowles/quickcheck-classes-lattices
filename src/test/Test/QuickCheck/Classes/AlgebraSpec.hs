{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.AlgebraSpec where

import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Test.Hspec
    ( Spec )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Algebra.PartialOrd
    ( partialOrdLaws )

spec :: Spec
spec = do
    testLawsMany @(Map Int (Set Int))
        [ partialOrdLaws
        ]
    testLawsMany @(Set Int)
        [ partialOrdLaws
        ]
    testLawsMany @[Int]
        [ partialOrdLaws
        ]
