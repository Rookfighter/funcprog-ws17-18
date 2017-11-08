module MediaTest (
    testLib
) where

import Media
import Test.QuickCheck

-- goodAlbums "Jens" testLib -- should return "Bar" album
-- goodAlbums "Stefan" testLib -- should return "Foo" album
-- albumDurations testLib -- should return "Foo":315 and "Bar":430

testLib = MediaLib [
    Album "Foo" [
        Track "A" "B" 100 [
            Rating "Stefan" VeryGood,
            Rating "Jens"   Bad
            ],
        Track "C" "D" 150 [
            Rating "Stefan" Good],
        Track "E" "F" 65 [
            Rating "Stefan" Medium,
            Rating "Jens"   Medium
            ]
        ],
    Album "Bar" [
        Track "G" "H" 120 [
            Rating "Stefan" Good,
            Rating "Jens"   Bad
        ],
        Track "I" "J" 80 [
            Rating "Stefan" Bad,
            Rating "Jens"   Good
            ],
        Track "K" "L" 200 [
            Rating "Stefan" VeryBad
            ],
        Track "M" "N" 30 [
            Rating "Stefan" Medium,
            Rating "Jens"   Good
            ]
        ]
    ]
