module FibTest (
    prop_Fib
) where

import Fib
import Test.QuickCheck

prop_Fib n = fib n == fib' n

test_Fib = forAll (choose (0,25)) prop_Fib
