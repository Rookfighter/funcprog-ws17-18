-- StackCalcCLI.hs
--
--     Author: Fabian Meyer
-- Created on: 13 Dec 2017

module StackCalcCLI where

import StackCalc

runCalc :: IO ()
runCalc = do
    putStrLn "Stack Calculater v1.21"
    putStrLn "----------------------"
    s <- putStack []
    stackCalcLoop s

stackCalcLoop :: [Int] -> IO ()
stackCalcLoop s = do
    ms <- stackCalcStep s
    case ms of
        Nothing -> return ()
        Just s -> stackCalcLoop s

putStack :: [Int] -> IO [Int]
putStack s = do
    putStrLn . show $ 0 : s
    return s

stackCalcStep :: [Int] -> IO (Maybe [Int])
stackCalcStep s = do
    putStrLn "> "
    ans <- getLine
    case ans of
        "exit" -> return Nothing
        cmd -> do
                s2 <- putStack . readCommand cmd $ s
                return . Just $ s2
