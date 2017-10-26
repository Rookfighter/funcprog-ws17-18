module Fib (
    fib,
    fib'
) where

-- naive implementation
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- infinite list of fibs
fibs' :: [Integer]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

fib' :: Int -> Integer
fib' n = fibs'!!n
