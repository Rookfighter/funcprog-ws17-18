module TicTacToe (
    Space(..),
    Board,
    GameState(..),
    newBoard,
    setSpace,
    state
) where

data Space = Empty | Cross | Circle
    deriving (Show, Eq)

type Board = [Space]

data GameState = CrossesWon | CirclesWon | InProgress | Tied
    deriving (Show)

-- creates a new empty board
newBoard :: Board
newBoard = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

setSpace_ :: Board -> Space -> Integer -> Board
setSpace_ [] s i = undefined
setSpace_ (x:xs) s 0 = s:xs
setSpace_ (x:xs) s i = x : setSpace_ xs s (i-1)

setSpace :: Board -> Space -> Integer -> Integer -> Board
setSpace b s x y = setSpace_ b s (y*3+x)

-- returns a list of each nth element in the list
each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)
-- map head (takeWhile (not(null)) (iterate (drop n) x))

state :: Board -> GameState
state b | hor   Cross  0 = CrossesWon
        | hor   Cross  1 = CrossesWon
        | hor   Cross  2 = CrossesWon
        | ver   Cross  0 = CrossesWon
        | ver   Cross  1 = CrossesWon
        | ver   Cross  2 = CrossesWon
        | diag1 Cross    = CrossesWon
        | diag2 Cross    = CrossesWon
        | hor   Circle 0 = CirclesWon
        | hor   Circle 1 = CirclesWon
        | hor   Circle 2 = CirclesWon
        | ver   Circle 0 = CirclesWon
        | ver   Circle 1 = CirclesWon
        | ver   Circle 2 = CirclesWon
        | diag1 Circle   = CirclesWon
        | diag2 Circle   = CirclesWon
        | (length . filter (== Empty) $ b) > 0 = InProgress
        | otherwise = Tied
        where hor v n = (length . takeWhile (==v) . drop (n*3) $ b) >= 3
              ver v n = (length . takeWhile (==v) . each 3 . drop n $ b) >= 3
              diag1 v = (length . takeWhile (==v) . each 4 $ b) >= 3
              diag2 v = (length . takeWhile (==v) . each 2 . take 6 . drop 2 $ b) >= 3
