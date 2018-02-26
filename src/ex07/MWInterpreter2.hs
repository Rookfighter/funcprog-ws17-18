-- MWInterpreter2.hs
--
--     Author: Fabian Meyer
-- Created On: 15 Jan 2018

module MWInterpreter2 where

import MWLexer
import MWParser
import qualified Data.Map.Strict as M

import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy

data Value = I Integer
           | B Bool
    deriving(Show, Eq)

type Memory = M.Map Id Value
-- Ret = State Memory (Maybe Value) = Memory -> (Maybe Value, Memory)
type Ret = MaybeT (State Memory) Value

runFile :: String -> IO (Maybe (Maybe Value, Memory))
runFile f = do
    s <- readFile f
    return . runString $ s

runString :: String -> Maybe (Maybe Value, Memory)
runString s = do
    p <- parseString s
    return $ eval p


-- evaluate a full program
eval :: Program -> (Maybe Value, Memory)
eval (Program stmts) = runState (runMaybeT (evalStmts stmts)) M.empty

-- evaluate a list of statements
evalStmts :: [Stmt] -> Ret
evalStmts stmts = foldr (\stmt ret -> (evalStmt stmt) >> ret) skip $ stmts

-- evaluate a single statement
evalStmt :: Stmt -> Ret
evalStmt whl@(While cond stmts) = do
    val <- evalExp cond
    case val of
        B v -> if v then evalStmts stmts >> evalStmt whl else skip
        _   -> skip
evalStmt (Asgn (Var ident) exp1) = do
    val <- evalExp exp1
    modify (\mem -> M.insert ident val mem)
    skip

-- evaluate expressions
evalExp :: Exp -> Ret
evalExp (If cond exp1 exp2) = do
    val <- evalExp cond
    case val of
        B v -> if v then evalExp exp1 else evalExp exp2
        _     -> mfail -- if condition is not a boolean fail
-- Both input expressions are arithmetic expressions!
evalExp (Cmp exp1 cmp exp2) =
    execCmp (evalAExp exp1) cmp (evalAExp exp2)
    where execCmp ret1 cmp ret2 = do {
              val1 <- ret1;
              val2 <- ret2;
              matchCmp val1 cmp val2
              }
          matchCmp (I i1) "<=" (I i2)  = return $ B (i1 <= i2)
          matchCmp (I i1) ">"  (I i2)  = return $ B (i1 > i2)
          matchCmp (I i1) "==" (I i2)  = return $ B (i1 == i2)
          matchCmp (I i1) "!=" (I i2)  = return $ B (i1 /= i2)
          matchCmp _ _ _               = mfail
evalExp (Inv exp1) = do
    val <- evalExp exp1
    case val of
        B v -> return . B . not $ v
        _   -> mfail
evalExp (AExp exp1) = evalAExp exp1

-- evaluate arithmetic expressions
evalAExp :: AExp -> Ret
evalAExp (Num num)   = lift . return $ I num
evalAExp (Var ident) = do
    mem <- lift $ get
    case M.lookup ident mem of
        Just val -> return val
        Nothing  -> mfail
evalAExp (Op exp1 op exp2) =
    execOp (evalAExp exp1) op (evalAExp exp2)
    where execOp ret1 op ret2 = do {
              val1 <- ret1;
              val2 <- ret2;
              matchOp val1 op val2}
          matchOp (I i1) "+" (I i2) = return $ I (i1 + i2)
          matchOp (I i1) "-" (I i2) = return $ I (i1 - i2)
          matchOp (I i1) "*" (I i2) = return $ I (i1 * i2)
          matchOp (I i1) "/" (I 0)  = mfail
          matchOp (I i1) "/" (I i2) = return $ I (i1 `div` i2)
          matchOp _ _ _             = mfail

mfail :: Monad m => MaybeT m a
mfail = MaybeT $ return Nothing

skip :: Ret
skip = return $ (I 0)

i2b :: Integer -> Bool
i2b 0 = False
i2b _ = True

b2i :: Bool -> Integer
b2i True = 1
b2i False = 0
