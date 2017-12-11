{-# LANGUAGE OverloadedStrings #-}

module RunEx04 where

import Vec2
import Picture
import PictureSvg as Ps
import PictureExt
import Graphics.Svg
import DragonCurve
import NumbersGame

svg :: Element -> Element
svg content = doctype <>
    with (svg11_
        (rect_ [Width_ <<- "100%", Height_ <<- "100%", Fill_ <<- "white"]
         <> content))
        [Width_ <<- "100", Height_ <<- "100", Fill_ <<- "white"]

mainEx04Pic :: IO ()
mainEx04Pic = do
    renderToFile "foobar.svg" (svg . Ps.draw_picture . move' (Vec2 40 70) . scale' 2 . dragon $ 15)

mainEx04Num :: IO ()
mainEx04Num = runGame
