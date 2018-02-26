{-#LANGUAGE GADTs#-}
-- SafeList.hs
--
--     Author: Fabian Meyer
-- Created On: 26 Feb 2018

module SafeList where

data SafeList a where
    Nil  :: SafeList ()
    Cons :: a -> SafeList a -> SafeList a


safeHead = undefined
safeDrop = undefined
safeAppend = undefined
