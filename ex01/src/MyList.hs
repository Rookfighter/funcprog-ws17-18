module MyList (
    head',
    tail',
    init',
    last',
    length',
    reverse',
    (++-),
    iterate',
    map',
    filter',
    intersperse',
    concat',
    zipWith',
    repeat',
    and',
    takeWhile',
    dropWhile',
    maximum'
) where

head' :: [a] -> a
head' []      = error "head' with empty list"
head' (x:xs)  = x

tail' :: [a] -> [a]
tail' []     = error "tail' with empty list"
tail' (x:xs) = xs

last' :: [a] -> a
last' []     = error "last' with empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "init' with empty list"
init' l  = reverse' (tail' (reverse' l))

length' :: [a] -> Int
length' []   = 0
length' (x:xs) = (length' xs) + 1

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:[]) = [x]
reverse' (x:xs) = (reverse' xs) ++- [x]

(++-) :: [a] -> [a] -> [a]
(++-) [] b = b
(++-) a [] = a
(++-) (a:as) b = a:(as ++- b)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:(iterate' f (f x))

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:[]) = [(f x)]
map' f (x:xs) = (f x):(map' f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x       = x:(filter' f xs)
                 | otherwise = filter' f xs

intersperse' :: a -> [a] -> [a]
intersperse' a []         = []
intersperse' a (x:[])     = [x]
intersperse' a (x:xs) = x:a:(intersperse' a xs)

concat' :: Foldable t => t [a] -> [a]
concat' = foldr (++-) []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] y = []
zipWith' f x [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

repeat' :: a -> [a]
repeat' x = x:(repeat' x)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x:(takeWhile' p xs)
                   | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                   | otherwise = x:xs

maximum' :: (Ord a) => [a] -> a
maximum' []  = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) | x > maximum' xs = x
                | otherwise       = maximum' xs
