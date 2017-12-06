-- TestUndup.hs
--
--     Author: Fabian Meyer
-- Created on: 26 Oct 2017

module TestUndup where

import Data.List
import Undup
import Test.QuickCheck

prop_UndupNub :: [Integer] -> Bool
prop_UndupNub xs = undup xs == nub xs

prop_UndupUnEq :: [Integer] -> Bool
prop_UndupUnEq xs = checkdup . undup $ xs
    where checkdup xs = and . map (\x -> (length . filter (==x) $ xs) == 1) $ xs
