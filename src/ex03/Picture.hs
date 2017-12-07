-- Picture.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

module Picture where

import Vec2

-- picture datatype which allows to compose several shapes
data Picture = Blank | Line [Vec2d] | Pol [Vec2d] |
    Circle Vec2d Double | Comp Picture Picture
    deriving (Show)

-- implement monoid typeclass for Picture
instance Monoid Picture where
    mempty = Blank
    mappend p Blank = p
    mappend Blank p = p
    mappend p1 p2 = Comp p1 p2

instance Transformable Picture where
    -- move given Picture by a vector
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
    -- scale given picture by a factor
    -- scale Blank simply returns Blank
    scale _ Blank = Blank
    -- scale Line
    scale fac (Line ps) = Line . map (scale fac) $ ps
    -- scale Polygon
    scale fac (Pol ps) = Pol . map (scale fac) $ ps
    -- scale Circle
    scale fac (Circle c r) = Circle (scale fac c) (r*fac)
    -- scale nested Pictures
    scale fac (Comp p1 p2) = Comp (scale fac p1) (scale fac p2)
    -- rotate Picture around given center with given angle
    -- rotating Blank does nothing
    rotate _ _ Blank = Blank
    -- rotate Line
    rotate c a (Line ps) = Line . map (rotate c a) $ ps
    -- rotatet Polygon
    rotate c a (Pol ps) = Pol . map (rotate c a) $ ps
    -- rotate Circle
    rotate c a (Circle c1 r) = Circle (rotate c a c1) r
    -- rotate nested Pictures
    rotate c a (Comp p1 p2) = Comp (rotate c a p1) (rotate c a p2)

-- define sqare triangle picture from ex sheet
square_tri = mappend (Pol [Vec2 0 0, Vec2 1 0, Vec2 1 1, Vec2 0 1])
             (Line [Vec2 0 1, Vec2 0.5 0, Vec2 1 1])

-- define square house with roof window from ex sheet
square_house = mappend (Pol [Vec2 0 1, Vec2 1 1, Vec2 1 2, Vec2 0 2]) .
               mappend (Line [Vec2 0 1, Vec2 0.5 0, Vec2 1 1]) .
               mappend (Line [Vec2 1 1, Vec2 0 2]) .
               mappend (Line [Vec2 0 1, Vec2 1 2]) $
               Circle (Vec2 0.5 0.70) 0.25
