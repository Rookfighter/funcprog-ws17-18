-- Protected.hs
--
--     Author: Fabian Meyer
-- Created on: 09 Jan 2018

module Protected where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

data ProtectedData a = ProtectedData String a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pass v) =
    if s == pass then Just v else Nothing

type Protected s a = MaybeT (Reader (ProtectedData s)) a

run :: ProtectedData s -> Protected s a -> Maybe a
run = undefined

access :: String -> Protected a a
access = undefined
