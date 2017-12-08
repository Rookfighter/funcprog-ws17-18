-- PictureExt.hs
--
--     Author: Fabian Meyer
-- Created on: 07 Dec 2017

module PictureExt where

import Vec2
import Picture

-- Applies the given function f to all points inside the given picture.
mapCoord :: (Vec2d -> Vec2d) -> Picture -> Picture
mapCoord f Blank = Blank
mapCoord f (Line xs) = Line . map f $ xs
mapCoord f (Pol xs) = Pol . map f $ xs
mapCoord f (Circle c r) = Circle (f c) r
mapCoord f (Comp p1 p2) = Comp (mapCoord f p1) (mapCoord f p2)

-- Applies the given function f to all points inside the given picture.
mapScalar :: (Double -> Double) -> Picture -> Picture
mapScalar f (Circle c r) = Circle c (f r)
mapScalar f (Comp p1 p2) = Comp (mapScalar f p1) (mapScalar f p2)
mapScalar _ p = p

-- New implementation of move using mapCoord.
move' :: Vec2d -> Picture -> Picture
move' v p = mapCoord (+v) p

-- New implementation of scale using mapCoord.
scale' :: Double -> Picture -> Picture
scale' fac p = mapCoord (scale fac) . mapScalar (*fac) $ p

-- New implementation of rotate using mapCoord.
rotate' :: Vec2d -> Double -> Picture -> Picture
rotate' c a p = mapCoord (rotate c a) p
