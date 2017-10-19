module WarmUpProps (
    prop_Mini,
    prop_Maxi,
    prop_Max3,
    prop_Med,
    test_WarmUp
) where

import WarmUp
import Data.List
import Test.QuickCheck

prop_Mini :: Int -> Int -> Bool
prop_Mini a b = (min a b) == (mini a b)

prop_Maxi :: Int -> Int -> Bool
prop_Maxi a b = (max a b) == (maxi a b)

prop_Max3 :: Int -> Int -> Int -> Bool
prop_Max3 a b c = ((Data.List.sort [a,b,c]) !! 2) == (max3 a b c)

prop_Med :: Int -> Int -> Int -> Bool
prop_Med a b c = ((Data.List.sort [a,b,c]) !! 1) == (med a b c)

test_WarmUp = do
    quickCheck prop_Mini
    quickCheck prop_Maxi
    quickCheck prop_Max3
    quickCheck prop_Med
