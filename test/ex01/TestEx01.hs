-- TestEx01.hs
--
--     Author: Fabian Meyer
-- Created on: 19 Oct 2017

module TestEx01 where

import TestWarmUp
import TestStackCalc
import Test.QuickCheck

testEx01 :: IO ()
testEx01 = do
    test_WarmUp
    test_StackCalc
