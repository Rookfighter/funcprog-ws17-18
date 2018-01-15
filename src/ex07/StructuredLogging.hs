-- StructuredLogging.hs
--
--     Author: Fabian Meyer
-- Created on: 15 Jan 2018

module StructuredLogging where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Data.Time.Clock.POSIX

data Item = Msg POSIXTime String
          | Section POSIXTime POSIXTime String [Item]
    deriving (Show,Eq)

type Log = [Item]
type Logging a = WriterT Log IO a

-- logs the messages s
logm :: Show t => t -> Logging ()
logm s = do
    ts <- lift $ getPOSIXTime
    tell $ return $ Msg ts (show s)

-- executes m and add its log in a section titled s
withSection :: String -> Logging a -> Logging a
withSection s m =
    pass $ g m
    where g m = do {
        ts1 <- lift $ getPOSIXTime;
        a <- m;
        ts2 <- lift $ getPOSIXTime;
        return (a, \l -> return $ Section ts1 ts2 s l)}

-- run logger to retrieve the result
runLogging :: Logging a -> IO (a, Log)
runLogging l = runWriterT l

printLogging :: Logging a -> IO ()
printLogging l = do
    r <- runLogging l
    putStrLn . show . snd $ r
    -- where g (Msg ts msg) = putStrLn "[Msg " ++ show ts ++ "] " ++ s
    --       g (Section ts1 ts2 s is) = do {
    --           putStrLn "[Sec " ++ show ts1 ++ "] " ++ s;
    --           mapM_ g is;
    --           putStrLn "[" ++ show ts2 ++ "]"}

test1 = do
    logm "Foo"
    logm "Bar"

test2 = do
    withSection "Hello" test1
    withSection "World" (do {logm "ghosts"; logm "everywhere"})
