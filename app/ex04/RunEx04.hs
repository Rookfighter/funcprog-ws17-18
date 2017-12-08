{-# LANGUAGE OverloadedStrings #-}

module RunEx04 where

import Vec2
import Picture
import PictureSvg as Ps
import PictureExt
import Graphics.Svg
import DragonCurve

svg :: Element -> Element
svg content = doctype <>
    with (svg11_
        (rect_ [Width_ <<- "100%", Height_ <<- "100%", Fill_ <<- "white"]
         <> content))
        [Width_ <<- "100", Height_ <<- "100", Fill_ <<- "white"]

mainEx04 :: IO ()
mainEx04 = do
    renderToFile "foobar.svg" (svg . Ps.draw_picture . move' (Vec2 40 70) . scale' 2 . dragon $ 15)
