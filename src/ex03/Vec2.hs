{-#LANGUAGE GADTs#-}
-- Vec2.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

module Vec2 where

data Vec2 a = Vec2 a a
  deriving (Show)

type Vec2f = Vec2 Float
type Vec2i = Vec2 Int
type Vec2d = Vec2 Double

-- introduce type class for transform operations
class Transformable a where
    move :: Vec2d -> a -> a
    scale :: Double -> a -> a
    rotate :: Vec2d -> Double -> a -> a

-- implement Num typeclass for Vec2
instance Num a => Num (Vec2 a) where
    (+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
    (-) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1-x2) (y1-y2)
    (*) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)
    abs (Vec2 x y) = Vec2 (abs x) (abs y)
    signum (Vec2 x y) = Vec2 (signum x) (signum y)
    fromInteger i = Vec2 (fromInteger i) (fromInteger i)

-- implement Eq typeclass for Vec2
instance Eq a => Eq (Vec2 a) where
    (==) (Vec2 x1 y1) (Vec2 x2 y2) = x1 == x2 && y1 == y2

-- implement Monoid typeclass for Vec2
instance Monoid a => Monoid (Vec2 a) where
    mappend (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (mappend x1 x2) (mappend y1 y2)
    mempty = Vec2 mempty mempty

instance a ~ Double => Transformable (Vec2 a) where
    -- translate vector v by d
    move d v = v + d
    -- scale vector by factor d
    scale d (Vec2 x y) = Vec2 (d*x) (d*y)
    -- rotate vector around center c by angle a (radian)
    -- first translate point into coord of c (rel vaector)
    -- then apply rotation matrix and translate back
    rotate c a p = c + (rmat (p - c))
        where rmat (Vec2 x y) = Vec2 (x * (cos a) - y * (sin a))
                                     (x * (sin a) + y * (cos a))
