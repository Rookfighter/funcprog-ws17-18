module FibTest (
    prop_Fib
) where

import Fib
import Test.QuickCheck

prop_Fib n | n > 0 && n < 25 = fib n == fib' n
           | otherwise = True
