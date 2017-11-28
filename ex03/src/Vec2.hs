-- Vec2.hs
--
--     Author: Fabian Meyer
-- Created on: 28 Nov 2017

module Vec2 where

data Vec2 a = Vec2 a a
  deriving (Show)

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
