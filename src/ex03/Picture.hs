-- Picture.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

module Picture where

import qualified Vec2 as V

-- Point type to make code more readable
type Point = V.Vec2 Double

-- picture datatype which allows to compose several shapes
data Picture = Blank | Line [Point] | Pol [Point] |
    Circle Point Double | Comp Picture Picture
    deriving (Show)

-- implement monoid typeclass for Picture
instance Monoid Picture where
    mempty = Blank
    mappend p Blank = p
    mappend Blank p = p
    mappend p1 p2 = Comp p1 p2

-- scale given picture by a factor
scale :: Double -> Picture -> Picture
-- scale Blank simply returns Blank
scale _ Blank = Blank
-- scale Line
scale fac (Line ps) = Line . map (V.scale fac) $ ps
-- scale Polygon
scale fac (Pol ps) = Pol . map (V.scale fac) $ ps
-- scale Circle
scale fac (Circle c r) = Circle (V.scale fac c) (r*fac)
-- scale nested Pictures
scale fac (Comp p1 p2) = Comp (scale fac p1) (scale fac p2)

-- move given Picture by a vector
move :: V.Vec2 Double -> Picture -> Picture
-- moving Blank does nothing
move _ Blank = Blank
-- move Line
move v (Line ps) = Line . map ((+) v) $ ps
-- move Polygon
move v (Pol ps) = Pol . map ((+) v) $ ps
-- move Circle
move v (Circle c r) = Circle (c + v) (r)
-- move nested Pictures
move v (Comp p1 p2) = Comp (move v p1) (move v p2)

-- rotates point around given center
-- first translate point into coord of c (rel vaector)
-- then apply rotation matrix and translate back
rotatep :: Point -> Double -> Point -> Point
rotatep c a p = c + (rmat (p - c))
    where rmat (V.Vec2 x y) = V.Vec2 (x * (cos a) - y * (sin a))
                                     (x * (sin a) + y * (cos a))

-- rotate Picture around given center with given angle
rotate :: Point -> Double -> Picture -> Picture
-- rotating Blank does nothing
rotate _ _ Blank = Blank
-- rotate Line
rotate c a (Line ps) = Line . map (rotatep c a) $ ps
-- rotatet Polygon
rotate c a (Pol ps) = Pol . map (rotatep c a) $ ps
-- rotate Circle
rotate c a (Circle c1 r) = Circle (rotatep c a c1) r
-- rotate nested Pictures
rotate c a (Comp p1 p2) = Comp (rotate c a p1) (rotate c a p2)

-- define sqare triangle picture from ex sheet
square_tri = mappend (Pol [V.Vec2 0 0, V.Vec2 1 0, V.Vec2 1 1, V.Vec2 0 1])
             (Line [V.Vec2 0 1, V.Vec2 0.5 0, V.Vec2 1 1])

-- define square house with roof window from ex sheet
square_house = mappend (Pol [V.Vec2 0 1, V.Vec2 1 1, V.Vec2 1 2, V.Vec2 0 2]) .
               mappend (Line [V.Vec2 0 1, V.Vec2 0.5 0, V.Vec2 1 1]) .
               mappend (Line [V.Vec2 1 1, V.Vec2 0 2]) .
               mappend (Line [V.Vec2 0 1, V.Vec2 1 2]) $
               Circle (V.Vec2 0.5 0.70) 0.25