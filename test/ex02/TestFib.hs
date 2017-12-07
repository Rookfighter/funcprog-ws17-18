-- TestFib.hs
--
--     Author: Fabian Meyer
-- Created on: 26 Oct 2017

module TestFib where

import Fib
import Test.QuickCheck

prop_Fib n = fib n == fib' n

test_Fib = do
    quickCheck (forAll (choose (0,25)) prop_Fib)
