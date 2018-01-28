-- MWInterpreter2.hs
--
--     Author: Fabian Meyer
-- Created On: 15 Jan 2018

module MWInterpreter2 where

import MWLexer
import MWParser
import qualified Data.Map.Strict as M

import Control.Monad.Trans.State.Lazy

data Value = I Integer
           | B Bool
    deriving(Show, Eq)

type Memory = M.Map Id Value
type  a = StateT Memory Maybe a

-- evaluate expressions
evalExp :: Exp -> Memory -> Value
evalExp (If eB e1 e2) m =
    if evalExp eB m /= 0 then
        evalExp e1 m
    else
        evalExp e2 m
evalExp (Cmp ae1 c ae2) m =
    g (evalAExp ae1 m) c (evalAExp ae2 m)
    where g v1 "<=" v2 = b2i (v1 <= v2)
          g v1 ">"  v2 = b2i (v1 >  v2)
          g v1 "==" v2 = b2i (v1 == v2)
          g v1 "!=" v2 = b2i (v1 /= v2)
          g v1 c v2 = error ("Invalid comparison operator " ++ c)
evalExp (Inv e) m = b2i (0 == evalExp e m)
evalExp (AExp ae) m = evalAExp ae m

-- evaluate arithmetic expressions
evalAExp :: AExp -> Memory -> Result Value
evalAExp (Num n) _ = Just Int n
evalAExp (Var i) m = case M.lookup i m of
        Just v  -> Just Int v -- return value of variable
        Nothing -> 0 -- nonexistent variable = 0
evalAExp (Op ae1 op ae2) m =
    g (evalAExp ae1 m) op (evalAExp ae2 m)
    where g v1 "+" v2 = Int $ v1 + v2
          g v1 "-" v2 = Int $ v1 - v2
          g v1 "/" 0  =
          g v1 "/" v2 = v1 `div` v2
          g v1 "*" v2 = v1 * v2
          g v1 op  v2 = error ("Invalid aritmetic operator " ++ op)

i2b :: Integer -> Bool
i2b 0 = False
i2b _ = True

b2i :: Bool -> Integer
b2i True = 1
b2i False = 0
