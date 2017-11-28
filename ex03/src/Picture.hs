-- Picture.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

module Picture where

import qualified Vec2 as V

-- helper typedefs for making code more readable
type Point = V.Vec2 Double
type Dim = V.Vec2 Double

-- picture datatype which allows to compose several shapes
data Picture = Blank | Line Point Point | Rect Point Dim | Circle Point Double |
    Child Picture Picture
    deriving (Show)

-- implement monoid typeclass for Picture
instance Monoid Picture where
    mempty = Blank
    mappend p Blank = p
    mappend Blank p = p
    mappend p1 p2 = Child p1 p2

-- scale given picture by a factor
scale :: Double -> Picture -> Picture
scale fac Blank = Blank
scale fac (Line p1 p2) = Line (V.scale fac p1) (V.scale fac p2)
scale fac (Rect p d) = Rect (V.scale fac p) (V.scale fac d)
scale fac (Circle c r) = Circle (V.scale fac c) (r*fac)
scale fac (Child p1 p2) = Child (scale fac p1) (scale fac p2)

-- define sqare triangle picture from ex sheet
square_tri = Child
                (Rect (V.Vec2 0 0) (V.Vec2 1 1))
                (Child
                    (Line (V.Vec2 0.5 0) (V.Vec2 0 1))
                    (Line (V.Vec2 0.5 0) (V.Vec2 1 1)))

-- define square house with roof window from ex sheet
square_house = mappend (Rect (V.Vec2 0 1) (V.Vec2 1 1)) .
               mappend (Line (V.Vec2 0.5 0) (V.Vec2 0 1)) .
               mappend (Line (V.Vec2 0.5 0) (V.Vec2 1 1)) .
               mappend (Line (V.Vec2 1 1) (V.Vec2 0 2)) .
               mappend (Line (V.Vec2 0 1) (V.Vec2 1 2)) $
               Circle (V.Vec2 0.5 0.70) 0.25
