{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text as T
import Vec2
import Picture as P
import Graphics.Svg

draw_picture :: Picture -> Element
-- draw_picture Blank =
draw_picture (P.Line (Vec2 x1 y1) (Vec2 x2 y2)) = path_ [ D_ <<- (mA x1 y1 <> lA x2 y2), Stroke_ <<- "black"]
draw_picture (P.Rect (Vec2 x y) (Vec2 w h)) = draw_picture (Line (Vec2 x y) (Vec2 (x+w) y))
    <> draw_picture (P.Line (Vec2 (x+w) y) (Vec2 (x+w) (y+h)))
    <> draw_picture (P.Line (Vec2 (x+w) (y+h)) (Vec2 x (y+h)))
    <> draw_picture (P.Line (Vec2 (x) (y+h)) (Vec2 x y))
draw_picture (P.Circle (Vec2 x y) r) = circle_ [Cx_ <<- (T.pack . show $ x), Cy_ <<- (T.pack . show $ y), R_ <<- (T.pack . show $ r), Stroke_ <<- "black"]
draw_picture (P.Child p1 p2) = draw_picture p1 <> draw_picture p2

square_tri_svg = draw_picture (P.scale 20 P.square_tri)
square_house_svg = draw_picture (P.scale 20 P.square_house)

svg :: Element -> Element
svg content = doctype <>
    with (svg11_
        (rect_ [Width_ <<- "100%", Height_ <<- "100%", Fill_ <<- "white"]
         <> content))
        [Width_ <<- "100", Height_ <<- "100", Fill_ <<- "white"]

main :: IO ()
main = do
    renderToFile "foobar.svg" (svg square_house_svg)
