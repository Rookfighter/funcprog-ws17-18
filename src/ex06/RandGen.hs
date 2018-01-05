-- RandGen.hs
--
--     Author: Fabian Meyer
-- Created On: 05 Jan 2018

module RandGen where

import Control.Monad.Trans.State.Lazy

type Random a = State Int a

fresh :: Random Int
fresh = return (rndSeries 1)

rndSeries :: Int -> Int
rndSeries x = (6364136223846793005 * x + 1442695040888963407) `mod` (2^32)

runPRNG :: Random a -> Int -> a
runPRNG r n = evalState (modify rndSeries >> r) n
