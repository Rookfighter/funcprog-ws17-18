module Undup (
    undup
) where

-- remove all duplicates from a list
undup :: Eq a => [a] -> [a]
undup [] = []
undup (x:xs) = x:(undup (filter (/= x) xs))
