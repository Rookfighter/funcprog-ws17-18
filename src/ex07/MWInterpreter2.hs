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
-- evalExp :: Exp -> Memory -> Value
-- evalExp (If eB e1 e2) m =
--     if evalExp eB m /= 0 then
--         evalExp e1 m
--     else
--         evalExp e2 m
-- evalExp (Cmp ae1 c ae2) m =
--     g (evalAExp ae1 m) c (evalAExp ae2 m)
--     where g v1 "<=" v2 = b2i (v1 <= v2)
--           g v1 ">"  v2 = b2i (v1 >  v2)
--           g v1 "==" v2 = b2i (v1 == v2)
--           g v1 "!=" v2 = b2i (v1 /= v2)
--           g v1 c v2 = error ("Invalid comparison operator " ++ c)
-- evalExp (Inv e) m = b2i (0 == evalExp e m)
-- evalExp (AExp ae) m = evalAExp ae m

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
