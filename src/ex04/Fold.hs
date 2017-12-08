-- Fold.hs
--
--     Author: Fabian Meyer
-- Created on: 06 Dec 2017

module Fold where

-- handmade foldr implementation
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op x0 [] = x0
foldr' op x0 (x:xs) = op x (foldr' op x0 xs)

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
    where g y ys | head ys == y = ys
                 | otherwise    = y : ys

-- handmade foldl implementation
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' op x0 [] = x0
foldl' op x0 (x:xs) = op (foldl' op x0 xs) x

-- handmade unfoldr implementation
unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f b = case f b of
    Nothing      -> []
    Just (a, b') -> a : unfoldr' f b'

-- map implementation using unfoldr'
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = unfoldr' g xs
    where g (y:ys) = Just (f y, ys)
          g []   = Nothing

-- iterate implementation using unfoldr'
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfoldr' g
    where g b = Just(b, f b)
