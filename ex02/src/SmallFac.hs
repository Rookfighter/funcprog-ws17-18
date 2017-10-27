module SmallFac (
    sfac,
    sfac'
) where

import Data.Numbers.Primes

sfac :: Integer -> Integer
sfac n = head (primeFactors n)

first' :: (a -> Bool) -> [a] -> a
first' p [] = error "first' called with empty list"
first' p (x:xs) | p x       = x
                | otherwise = first' p xs

sfac' :: Integer -> Integer
sfac' n = first' (\x -> (mod n x) == 0) primes
