-- Undup.hs
--
--     Author: Fabian Meyer
-- Created on: 26 Oct 2017

module Undup where

-- remove all duplicates from a list
undup :: Eq a => [a] -> [a]
undup [] = []
undup (x:xs) = x:(undup (filter (/= x) xs))
