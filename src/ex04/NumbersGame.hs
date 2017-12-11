-- Fold.hs
--
--     Author: Fabian Meyer
-- Created on: 06 Dec 2017

module NumbersGame where

import System.Random

parseAnswer :: String -> Int -> Maybe Int
parseAnswer "greater" n = Just . floor $ (fromIntegral n + ((100 - fromIntegral n) / 2))
parseAnswer "smaller" n = Just . floor $ (fromIntegral n / 2)
parseAnswer "yes" n = Just n
parseAnswer _ _ = Nothing

askNum :: Int -> IO (Maybe Int)
askNum n = do
    putStrLn ("Is it " ++ show n ++ "?")
    ans <- getLine
    return . parseAnswer ans $ n

askNumLoop :: Int -> IO (Maybe Int) -> IO (Maybe Int)
askNumLoop n1 io = do
    mn2 <- io
    case mn2 of
        Just n2 -> if n2 == n1 then return . Just $ n1
                   else askNumLoop n2 . askNum $ n2
        Nothing -> return Nothing

runGame :: IO ()
runGame = do
    guess <- getStdRandom . randomR $ (1,100)
    msol <- askNumLoop (-1) . return . Just $ guess
    case msol of
        Just n -> putStrLn ("Found " ++ show n ++ "!")
        Nothing -> putStrLn "No solution found."
