{-# LANGUAGE OverloadedStrings #-}

module Run where

import System.Random
import qualified Vec2 as V
import qualified Picture as P
import qualified PictureSvg as Ps
import Graphics.Svg
import DragonCurve


houses = [P.square_house, P.square_house, P.square_house, P.square_house, P.square_house]
housings g = Ps.draw_picture . foldr rand_house mempty $ houses
    where rand_house = scale_house (random g)
          scale_house (s, _) h = P.move (V.Vec2 (s*20) 0) . mappend (P.move (V.Vec2 0 (20.0-s)) . P.scale (s*20.0) $ h)


svg :: Element -> Element
svg content = doctype <>
    with (svg11_
        (rect_ [Width_ <<- "100%", Height_ <<- "100%", Fill_ <<- "white"]
         <> content))
        [Width_ <<- "100", Height_ <<- "100", Fill_ <<- "white"]

mymain :: IO ()
mymain = do
    g <- getStdGen
    renderToFile "foobar.svg" (svg . Ps.draw_picture . P.move (V.Vec2 40 70) . P.scale 2 . dragon' $ 15)
