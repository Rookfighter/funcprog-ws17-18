-- TestEx02.hs
--
--     Author: Fabian Meyer
-- Created on: 26 Oct 2017

module TestEx02 where

import TestFib
import TestUndup
import Test.QuickCheck

testEx02 :: IO ()
testEx02 = do
    test_Fib
    test_Undup
