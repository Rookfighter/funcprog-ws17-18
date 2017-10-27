module MediaTest (
    testLib
) where

import Media

testLib = MediaLib [
    Album "Foo" [
        Track "A" "B" 100,
        Track "C" "D" 150,
        Track "E" "F" 65
    ],
    Album "Bar" [
        Track "G" "H" 120,
        Track "I" "J" 80,
        Track "K" "L" 200,
        Track "M" "N" 30
        ]
    ]

    [
    Rating (Track "A" "B" 100) "Stefan" VeryGood,
    Rating (Track "A" "B" 100) "Jens"   Bad,
    Rating (Track "E" "F" 100) "Jens"   Medium,
    Rating (Track "C" "D" 150) "Stefan" Good,
    Rating (Track "E" "F" 65)  "Stefan" Medium,
    Rating (Track "I" "J" 80)  "Stefan" Bad,
    Rating (Track "K" "L" 200) "Stefan" VeryBad,
    Rating (Track "M" "N" 30)  "Stefan" Medium
    ]
