-- TestTicTacToe.hs
--
--     Author: Fabian Meyer
-- Created on: 26 Oct 2017

module TestTicTacToe where

import TicTacToe

-- hor win
testBoard1 = [Cross,  Cross,  Cross,
             Empty,  Circle, Empty,
             Empty,  Empty,  Circle]

-- vert win
testBoard2 = [Empty,  Cross,  Empty,
              Empty,  Cross,  Circle,
              Empty,  Cross,  Circle]

-- diag 0 win
testBoard3 = [Circle, Cross,  Empty,
              Empty,  Circle, Cross,
              Empty,  Cross,  Circle]

-- diag 1 win
testBoard4 = [Circle, Empty, Cross,
              Empty,  Cross, Circle,
              Cross,  Empty, Circle]

 -- InProgress
testBoard5 = [Circle, Empty, Cross,
              Empty,  Empty, Circle,
              Cross,  Empty, Circle]

 -- Tied
testBoard6 = [Circle, Circle, Cross,
              Cross,  Circle, Circle,
              Circle, Cross,  Cross]
