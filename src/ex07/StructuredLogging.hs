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
log :: Show t => t -> Logging ()
log s = do
    ts <- lift $ getPOSIXTime
    tell $ return $ Msg ts (show s)

-- executes m and add its log in a section titled s
withSection :: String -> Logging a -> Logging a
withSection s m = do
    ts1 <- lift $ getPOSIXTime
    a <- m
    ts2 <- lift $ getPOSIXTime
    pass $ return (a, \l -> return $ Section ts1 ts2 s l)


-- run logger to retrieve the result
runLogging :: Logging a -> IO (a, Log)
runLogging l = runWriterT l
