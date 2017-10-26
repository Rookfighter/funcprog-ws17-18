module WarmUp (
    mini,
    maxi,
    max3,
    med
) where

-- compute minimum of 2 ints
mini :: Int -> Int -> Int
mini a b | a <= b    = a
         | otherwise = b

-- compute maximum of 2 ints
maxi :: Int -> Int -> Int
maxi a b | a >= b    = a
         | otherwise = b

-- compute maximum of 3 ints
max3 :: Int -> Int -> Int -> Int
max3 a b c = maxi (maxi a b) (maxi a c)

-- compute median of 3 ints
med :: Int -> Int -> Int -> Int
med a b c | a >= b && a <= c = a
          | a >= c && a <= b = a
          | b >= a && b <= c = b
          | b >= c && b <= a = b
          | c >= b && c <= a = c
          | c >= a && c <= b = c
