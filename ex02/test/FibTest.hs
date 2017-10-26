module FibTest (
    prop_Fib
) where

import Fib

prop_Fib n = fib n == fib' n
