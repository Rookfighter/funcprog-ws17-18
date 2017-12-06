-- PictureSvg.hs
--
--     Author: Fabian Meyer
-- Created on: 29 Nov 2017

module PictureSvg where

import qualified Vec2 as V
import qualified Picture as P
import qualified Data.Text as T
import Graphics.Svg

-- define function to draw a Picture on a SVG
draw_picture :: P.Picture -> Element
-- draw Blank element
draw_picture P.Blank = mempty
-- draw Line element
draw_picture (P.Line ps) =
    foldr (<>) mempty . zipWith draw_line ps $ tail ps
      where draw_line (V.Vec2 x1 y1) (V.Vec2 x2 y2) = path_ [ D_ <<- (mA x1 y1 <> lA x2 y2), Stroke_ <<- T.pack "black"]
-- draw Polygon element
draw_picture (P.Pol ps) = draw_picture . P.Line $ (ps ++ [head ps])
-- draw Circle element
draw_picture (P.Circle (V.Vec2 x y) r) = circle_ [Cx_ <<- (T.pack . show $ x),
    Cy_ <<- (T.pack . show $ y),
    R_ <<- (T.pack . show $ r),
    Stroke_ <<- T.pack "black"]
-- draw nested Picture
draw_picture (P.Comp p1 p2) = draw_picture p1 <> draw_picture p2

-- predefined square triangle picture
square_tri_svg = draw_picture (P.scale 20 P.square_tri)
-- predefined square house
square_house_svg = draw_picture (P.scale 20 P.square_house)
