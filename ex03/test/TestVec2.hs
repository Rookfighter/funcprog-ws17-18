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

-- -- check mempty is identity of mappend
-- prop_Vec2_memtpy :: Vec2 Integer -> Bool
-- prop_Vec2_memtpy v = mappend mempty v == v && mappend v mempty == v
--
-- -- check mappend is associative
-- prop_Vec2_assoc :: Vec2 Integer -> Vec2 Integer -> Vec2 Integer -> Bool
-- prop_Vec2_assoc v1 v2 v3 = mappend v1 (mappend v2 v3) == mappend (mappend v1 v2) v3
--
-- -- check mconcat behaves same as foldr
-- prop_Vec2_mconcat :: [Vec2 Integer] -> Bool
-- prop_Vec2_mconcat vs = mconcat vs == foldr mappend mempty vs

test_Vec2 = do
    quickCheck prop_Vec2_signum
    quickCheck prop_Vec2_add
    quickCheck prop_Vec2_mul
    -- prop_Vec2_memtpy
    -- prop_Vec2_assoc
    -- prop_Vec2_mconcat
