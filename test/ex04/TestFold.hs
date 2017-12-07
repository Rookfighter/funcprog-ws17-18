-- TestFold.hs
--
--     Author: Fabian Meyer
-- Created on: 07 Dec 2017

module TestFold where

import Fold
import Test.QuickCheck

prop_or :: [Bool] -> Bool
prop_or xs = or xs == or' xs

prop_filter :: [Int] -> Int -> Bool
prop_filter xs c = filter (>c) xs == filter' (>c) xs

prop_map :: [Int] -> Int -> Bool
prop_map xs a = map (*a) xs == map' (*a) xs

test_Fold = do
    quickCheck prop_or
    quickCheck prop_filter
    quickCheck prop_map
