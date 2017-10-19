module StackCalcProps (
    prop_Pop,
    prop_Dup,
    prop_Add,
    prop_Sub,
    prop_Mul,
    prop_Neg,
    prop_Push,
    test_StackCalc
) where

import StackCalc
import Test.QuickCheck

prop_Pop [] = length (pop []) == 0
prop_Pop s = length (pop s) == (length s) - 1

prop_Dup [] = last (dup []) == 0 &&
              length (dup []) == 1
prop_Dup s  = last (dup s) == last s &&
              length (dup s) == (length s) + 1

prop_Add []  = last (add []) == 0 &&
               length (add []) == 1
prop_Add [x] = last (add [x]) == x &&
               length (add [x]) == 1
prop_Add s   = last (add s) == (last s) + (last (init s)) &&
               length (add s) == (length s) - 1

prop_Sub []  = last (sub []) == 0 &&
               length (sub []) == 1
prop_Sub [x] = last (sub [x]) == x &&
               length (sub [x]) == 1
prop_Sub s   = last (sub s) == (last s) - (last (init s)) &&
               length (sub s) == (length s) - 1

prop_Mul []  = last (mul []) == 0 &&
               length (mul []) == 1
prop_Mul [x] = last (mul [x]) == 0 &&
               length (mul [x]) == 1
prop_Mul s   = last (mul s) == (last s) * (last (init s)) &&
               length (mul s) == (length s) - 1

prop_Neg [] = last (neg []) == 0 &&
              length (neg []) == 1
prop_Neg s  = last (neg s) == -(last s) &&
              length (neg s) == length s

prop_Push s x = last (push s x) == x &&
                length (push s x) == (length s) + 1

test_StackCalc = do
    quickCheck prop_Pop
    quickCheck prop_Dup
    quickCheck prop_Add
    quickCheck prop_Sub
    quickCheck prop_Mul
    quickCheck prop_Neg
    quickCheck prop_Push
