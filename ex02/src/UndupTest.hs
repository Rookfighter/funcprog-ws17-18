module UndupTest (
    prop_UndupNub,
    prop_UndupUnEq
) where

import Data.List
import Undup
import Test.QuickCheck

prop_UndupNub :: [Integer] -> Bool
prop_UndupNub xs = undup xs == nub xs

prop_UndupUnEq :: [Integer] -> Bool
prop_UndupUnEq xs = checkdup . undup $ xs
    where checkdup xs = and . map (\x -> (length . filter (==x) $ xs) == 1) $ xs
