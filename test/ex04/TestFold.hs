-- TestFold.hs
--
--     Author: Fabian Meyer
-- Created on: 07 Dec 2017

module TestFold where

import Fold
import Data.List
import Test.QuickCheck

prop_or' :: [Bool] -> Bool
prop_or' xs = or xs == or' xs

prop_filter' :: [Int] -> Int -> Bool
prop_filter' xs c = filter (>c) xs == filter' (>c) xs

prop_map' :: [Int] -> Int -> Bool
prop_map' xs a = map (*a) xs == map' (*a) xs

prop_map'' :: [Int] -> Int -> Bool
prop_map'' xs a = map (*a) xs == map'' (*a) xs

prop_iterate' :: Int -> Int -> Bool
prop_iterate' a l = (take l . iterate (*2) $ a) == (take l . iterate' (*2) $ a)

test_Fold = do
    quickCheck prop_or'
    quickCheck prop_filter'
    quickCheck prop_map'
    quickCheck prop_map''
    quickCheck prop_iterate'
