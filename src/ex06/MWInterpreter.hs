-- MWInterpreter.hs
--
--     Author: Fabian Meyer
-- Created On: 09 Jan 2018

module MWInterpreter where

import MWLexer
import MWParser
import Data.Map.Strict

type Value = Integer
type Memory = Map Id Value

eval :: Program -> Memory
eval = undefined
