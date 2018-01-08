-- Memoization.hs
--
--     Author: Fabian Meyer
-- Created On: 08 Jan 2018

module Memoization where

import Data.Map.Strict
import Control.Monad.Trans.State.Lazy

memoize :: (a -> b) -> a -> State a b
memoize f a = put a >> return (f a)
