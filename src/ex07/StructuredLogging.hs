-- StructuredLogging.hs
--
--     Author: Fabian Meyer
-- Created on: 15 Jan 2018

module StructuredLogging where

import Control.Monad.Trans.Writer.Strict

data Item = Msg String
          | Section String [Item]
    deriving (Show,Eq)

type Log = [Item]
type Logging a = Writer Log a

-- logs the messages s
log :: Show t => t -> Logging ()
log = tell . return . Msg . show

-- executes m and add its log in a section titled ‘s‘.
withSection :: String -> Logging a -> Logging a
withSection s m =
    censor (\l -> return $ Section s l)  m

-- run logger to retrieve the result
runLogging :: Logging a -> (a, Log)
runLogging l = runWriter l
