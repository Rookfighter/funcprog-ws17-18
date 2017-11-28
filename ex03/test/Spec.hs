-- Spec.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

import Test.QuickCheck
import TestVec2

main :: IO ()

main = do
    quickCheck prop_Vec2_signum
