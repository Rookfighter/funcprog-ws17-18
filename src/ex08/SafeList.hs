{-#LANGUAGE GADTs#-}
-- SafeList.hs
--
--     Author: Fabian Meyer
-- Created On: 26 Feb 2018

module SafeList where

data Empty;
data NonEmpty;

data SafeList a b where
    Nil  :: SafeList a Empty
    Cons :: a -> SafeList a b -> SafeList a NonEmpty


innerStr :: Show a => SafeList a b -> String
innerStr (Cons x (Cons y ys)) = show x ++ "," ++ innerStr (Cons y ys)
innerStr (Cons x Nil)         = show x

instance Show a => Show (SafeList a b) where
    show Nil = "[]"
    show xs  = "[" ++ innerStr xs ++ "]"

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons x _) = x


safeDrop :: Int -> SafeList a NonEmpty -> SafeList a NonEmpty
safeDrop i (Cons x (Cons y ys))  | i > 0 = safeDrop (i-1) (Cons y ys)
safeDrop i (Cons x xs)                   = Cons x xs

safeAppend :: SafeList a NonEmpty -> SafeList a NonEmpty -> SafeList a NonEmpty
safeAppend (Cons x Nil) ys = Cons x ys
safeAppend (Cons x (Cons y ys)) zs = Cons x $ safeAppend (Cons y ys) zs
