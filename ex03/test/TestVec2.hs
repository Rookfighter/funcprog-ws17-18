-- TestVec2.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

module TestVec2 where

import Test.QuickCheck
import Vec2

instance Arbitrary a => Arbitrary (Vec2 a) where
    arbitrary = arbitrary >>= \x -> arbitrary >>= \y -> return  $ Vec2 x y

prop_Vec2_signum :: Vec2 Integer -> Bool
prop_Vec2_signum x = (abs x) * (signum x) == x
