-- MWInterpreter.hs
--
--     Author: Fabian Meyer
-- Created On: 09 Jan 2018

module MWInterpreter where

import MWLexer
import MWParser
import qualified Data.Map.Strict as M

type Value = Integer
type Memory = M.Map Id Value

runFile :: String -> IO (Maybe Memory)
runFile f = do
    s <- readFile f
    return . runString $ s

runString :: String -> Maybe Memory
runString s = do
    p <- parseString s
    return . eval $ p


-- evaluate a full program
eval :: Program -> Memory
eval (Program stmts) = evalStmts stmts M.empty

-- evaluate a list of statements
evalStmts :: [Stmt] -> Memory -> Memory
-- evalStmts [] = m
-- evalStmts (x:xs) m = evalStmts xs (evalStmt x m)
evalStmts stmts m = foldr evalStmt m . reverse $ stmts

-- evaluate a single statement
evalStmt :: Stmt -> Memory -> Memory
evalStmt w@(While eB stmts) m =
    if evalExp eB m /= 0 then
        evalStmt w (evalStmts stmts m) --eval loop body and start recursion
    else
        m -- skip, just return current memory
evalStmt (Asgn (Var i) e) m =
    M.insert i (evalExp e m) m

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
evalAExp :: AExp -> Memory -> Value
evalAExp (Num n) _ = n
evalAExp (Var i) m = case M.lookup i m of
        Just v  -> v -- return value of variable
        Nothing -> 0 -- nonexistent variable = 0
evalAExp (Op ae1 op ae2) m =
    g (evalAExp ae1 m) op (evalAExp ae2 m)
    where g v1 "+" v2 = v1 + v2
          g v1 "-" v2 = v1 - v2
          g v1 "/" v2 = v1 `div` v2
          g v1 "*" v2 = v1 * v2
          g v1 op  v2 = error ("Invalid aritmetic operator " ++ op)

b2i :: Bool -> Integer
b2i True = 1
b2i False = 0
