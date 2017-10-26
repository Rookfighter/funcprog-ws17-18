import FibTest
import UndupTest
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck prop_Fib
    quickCheck prop_UndupNub
    quickCheck prop_UndupUnEq
