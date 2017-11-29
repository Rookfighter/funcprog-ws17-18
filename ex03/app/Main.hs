{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified PictureSvg as P
import Graphics.Svg

svg :: Element -> Element
svg content = doctype <>
    with (svg11_
        (rect_ [Width_ <<- "100%", Height_ <<- "100%", Fill_ <<- "white"]
         <> content))
        [Width_ <<- "100", Height_ <<- "100", Fill_ <<- "white"]

main :: IO ()
main = do
    renderToFile "foobar.svg" (svg P.square_house_svg)
