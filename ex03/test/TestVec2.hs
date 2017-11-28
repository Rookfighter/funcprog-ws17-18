-- TestVec2.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

module TestVec2 where

import Test.QuickCheck
import Vec2

-- implement Arbitraty typeclass for Vec2
instance Arbitrary a => Arbitrary (Vec2 a) where
    arbitrary = arbitrary >>= \x -> arbitrary >>= \y -> return  $ Vec2 x y

-- check signum property
prop_Vec2_signum :: Vec2 Integer -> Bool
prop_Vec2_signum v = (abs v) * (signum v) == v

-- check addition works as expected
prop_Vec2_add :: Vec2 Integer -> Vec2 Integer -> Bool
prop_Vec2_add v1 v2 = ceq v1 v2 (v1+v2)
    where ceq (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) = x1+x2 == x3 && y1+y2 == y3

-- check multiplication works element-wise
prop_Vec2_mul :: Vec2 Integer -> Vec2 Integer -> Bool
prop_Vec2_mul v1 v2 = ceq v1 v2 (v1*v2)
    where ceq (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) = x1*x2 == x3 && y1*y2 == y3

test_Vec2 = do
    quickCheck prop_Vec2_signum
    quickCheck prop_Vec2_add
    quickCheck prop_Vec2_mul
