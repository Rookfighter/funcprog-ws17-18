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

-- returns first element of list
head' :: [a] -> a
head' []      = error "head' with empty list"
head' (x:xs)  = x

-- returns list without first element
tail' :: [a] -> [a]
tail' []     = error "tail' with empty list"
tail' (x:xs) = xs

-- returns last element of list
last' :: [a] -> a
last' []     = error "last' with empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

-- returns list without last element
init' :: [a] -> [a]
init' [] = error "init' with empty list"
init' l  = reverse' (tail' (reverse' l))

-- returns number of elements in list
length' :: [a] -> Int
length' []   = 0
length' (x:xs) = (length' xs) + 1

-- reverts the list
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = (reverse' xs) ++- [x]

-- appends a list to another
(++-) :: [a] -> [a] -> [a]
(++-) [] b = b
(++-) a [] = a
(++-) (a:as) b = a:(as ++- b)

-- creates infinite list of elements x where f is applied
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:(iterate' f (f x))

-- returns list with f applied to all elements in list
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

-- returns list of elements in which f evaluates to True
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x       = x:(filter' f xs)
                 | otherwise = filter' f xs

-- returns list with element a between every element of list
intersperse' :: a -> [a] -> [a]
intersperse' a []         = []
intersperse' a (x:[])     = [x]
intersperse' a (x:xs) = x:a:(intersperse' a xs)

--
concat' :: Foldable t => t [a] -> [a]
concat' = foldr (++-) []

-- returns list of pairwise matched elements of x and y
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] y = []
zipWith' f x [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

-- creates infinite list of element x
repeat' :: a -> [a]
repeat' x = x:(repeat' x)

-- collapses Bool list by conjunction
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

-- retrieves longest series of elements where p evaluates to True
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x:(takeWhile' p xs)
                    | otherwise = []

-- retrieves elements which would are not returned by takeWHile'
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

-- finds maximum element of list
maximum' :: (Ord a) => [a] -> a
maximum' []  = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) | x > maximum' xs = x
                | otherwise       = maximum' xs
