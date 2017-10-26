module UndupTest (
    prop_UndupNub,
    prop_UndupUnEq
) where

import Data.List
import Undup

prop_UndupNub :: [Integer] -> Bool
prop_UndupNub l = undup l == nub l

prop_UndupUnEq :: [Integer] -> Bool
prop_UndupUnEq l = and (map (\x -> length (filter (==x) l) == 1) l)
