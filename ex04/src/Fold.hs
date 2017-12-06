-- Fold.hs
--
--     Author: Fabian Meyer
-- Created on: 06 Dec 2017

module Fold where

-- handmade foldr implementation
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op x0 [] = x0
foldr' op x0 (x:xs) = op x0 (foldr op x0 xs)

-- Returns True if at least one element is True in given list, else False.
-- Using custom foldr'.
or' :: [Bool] -> Bool
or' xs = foldr' (||) False xs

-- Returns list of elements for which given predictate returns True.
-- Using custom foldr'.
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr' g [] xs
    where g x xs | p x       = x : xs
                 | otherwise = xs

-- Maps elements into different domain with given function.
-- Using custom foldr'.
map' :: (a -> b) -> [a] -> [b]
map' f as = foldr' g [] as
    where g a bs = (f a) : bs

-- Removes consecutive duplicates in the whole list.
-- Using custom foldr'.
remdups' :: Eq a => [a] -> [a]
remdups' xs = foldr' g [] xs
    where g x xs | head xs == x = xs
                 | otherwise    = x : xs