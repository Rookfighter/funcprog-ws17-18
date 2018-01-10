-- Protected.hs
--
--     Author: Fabian Meyer
-- Created on: 09 Jan 2018

module Protected where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

data ProtectedData a = ProtectedData String a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pass v) =
    if s == pass then Just v else Nothing

type Protected s a = MaybeT (Reader (ProtectedData s)) a

myPD = ProtectedData "foo" "You are great!"
myNumPD = ProtectedData "foo" 42

numpd :: String -> Protected Integer Float
numpd s = do
    v <- access s
    return $ fromIntegral v

run :: ProtectedData s -> Protected s a -> Maybe a
run pd p =
    runReader (runMaybeT p) pd

access :: String -> Protected a a
access s = do
    pd <- lift $ ask
    case accessData s pd of
        Just v  -> return v
        Nothing -> fail ""
