-- NumbersGame.hs
--
--     Author: Fabian Meyer
-- Created on: 06 Dec 2017

module NumbersGame where

import System.Random

-- calcs next number as middle between upper and lower bound
calcNum :: Int -> Int -> Int
calcNum u l =  floor (fromIntegral(l) + fromIntegral(u-l) / 2)

-- parses the received answer
-- greater, smaller, yes are possible
parseAnswer :: String -> (Int, Int, Int) -> Maybe (Int, Int, Int)
parseAnswer "greater" (n, u, l) = Just (calcNum u n, u ,n)
parseAnswer "smaller" (n, u, l) = Just (calcNum n l, n, l)
parseAnswer "yes" (n, u, l) = Just (n, u, l)
parseAnswer _ _ = Nothing

-- asks if the found number is correct
-- passes the given answer to apropriate handler
askNum :: (Int, Int, Int) -> IO (Maybe (Int, Int, Int))
askNum (n, u, l) = do
    putStrLn ("Is it " ++ show n ++ "?")
    ans <- getLine
    return . parseAnswer ans $ (n, u, l)

-- implements a loop like behaviour which only returns if correct number was Found
-- or program was aborted
askNumLoop :: (Int, Int, Int) -> IO (Maybe (Int, Int, Int)) -> IO (Maybe Int)
askNumLoop (n1, u1, l1) io = do
    mn2 <- io
    case mn2 of
        Just (n2, u2, l2) -> if n2 == n1 then return . Just $ n1
                             else askNumLoop (n2, u2, l2) . askNum $ (n2, u2, l2)
        Nothing -> return Nothing

-- main function of the game
-- creates initial random guess and starts loop
runGame :: IO ()
runGame = do
    guess <- getStdRandom . randomR $ (1,100)
    msol <- askNumLoop (-1, 100, 1) . return . Just $ (guess, 100, 1)
    case msol of
        Just n -> putStrLn ("Found " ++ show n ++ "!")
        Nothing -> putStrLn "No solution found."
