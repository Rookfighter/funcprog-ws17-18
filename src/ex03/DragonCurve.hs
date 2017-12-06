-- DragonCurve.hs
--
--     Author: Fabian Meyer
-- Created on: 29 Nov 2017

module DragonCurve where

import Vec2 as V
import Picture

-- implement dragon curve recursively
dragons :: Int -> [Point] -> [Point]
dragons _ [] = []
dragons 0 xs = xs
dragons i xs = dragons (i-1) . (++) xs . reverse . map (rotatep (last xs) (pi/2)) . init $ xs
-- calc dragon curve of given iteration
dragon :: Int -> Picture
dragon n = Line . dragons n $ [Vec2 0 0, Vec2 1 0]
