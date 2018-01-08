-- Memoization.hs
--
--     Author: Fabian Meyer
-- Created On: 08 Jan 2018

module Memoization where

import Data.Map.Strict
import Control.Monad.Trans.State.Lazy

memoize :: (Int -> Int) -> Int -> State Int Int
memoize f n = put n >> return (f n)

-- memof :: (A -> State A B)
-- memof = memoize f
