{-# LANGUAGE OverloadedStrings #-}

module RunEx03 where

import System.Random
import  Vec2 as V
import Picture
import PictureSvg as Ps
import Graphics.Svg
import DragonCurve


houses = [square_house, square_house, square_house, square_house, square_house]
housings g = Ps.draw_picture . foldr rand_house mempty $ houses
    where rand_house = scale_house (random g)
          scale_house (s, _) h = move (Vec2 (s*20) 0) . mappend (move (Vec2 0 (20.0-s)) . V.scale (s*20.0) $ h)


svg :: Element -> Element
svg content = doctype <>
    with (svg11_
        (rect_ [Width_ <<- "100%", Height_ <<- "100%", Fill_ <<- "white"]
         <> content))
        [Width_ <<- "100", Height_ <<- "100", Fill_ <<- "white"]

mainEx03 :: IO ()
mainEx03 = do
    g <- getStdGen
    renderToFile "foobar.svg" (svg . Ps.draw_picture . move (V.Vec2 40 70) . V.scale 2 . dragon $ 15)
