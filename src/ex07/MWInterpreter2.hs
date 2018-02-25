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

-- evaluate expressions
evalExp :: Exp -> Ret -> Ret
evalExp (If cond exp1 exp2) ret = do
    val <- evalExp cond ret
    case val of
        B val -> if val then evalExp exp1 ret else evalExp exp2 ret
        _     -> mfail -- if condition is not a boolean fail
-- Both input expressions are arithmetic expressions!
evalExp (Cmp exp1 cmp exp2) ret =
    execCmp (evalAExp exp1 ret) cmp (evalAExp exp2 ret)
    where execCmp ret1 cmp ret2 = do {
              val1 <- ret1;
              val2 <- ret2;
              matchCmp val1 cmp val2
              }
          matchCmp (I i1) "<=" (I i2)  = return $ B (i1 <= i2)
          matchCmp (I i1) ">" (I i2)   = return $ B (i1 > i2)
          matchCmp (I i1) "==" (I i2)  = return $ B (i1 == i2)
          matchCmp (I i1) "!=" (I i2)  = return $ B (i1 /= i2)
          matchCmp _ _ _               = mfail
evalExp (Inv exp1) ret = do
    val <- evalExp exp1 ret
    case val of
        B v -> return . B . not $ v
        _   -> mfail
evalExp (AExp exp1) ret = evalAExp exp1 ret

-- evaluate arithmetic expressions
evalAExp :: AExp -> Ret -> Ret
evalAExp (Num num)   ret = fmap (\_ -> I num) ret
evalAExp (Var ident) _   = do
    mem <- lift $ get
    case M.lookup ident mem of
        Just val -> return val
        Nothing  -> mfail
evalAExp (Op exp1 op exp2) ret =
    execOp (evalAExp exp1 ret) op (evalAExp exp2 ret)
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

i2b :: Integer -> Bool
i2b 0 = False
i2b _ = True

b2i :: Bool -> Integer
b2i True = 1
b2i False = 0
