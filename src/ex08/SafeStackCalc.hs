{-#LANGUAGE GADTs#-}
-- SafeStackCalc.hs
--
--     Author: Fabian Meyer
-- Created On: 27 Feb 2018

module SafeStackCalc where

data None;

data SProg a b where
    Nil  :: SProg Int None
    Noop :: SProg a b -> SProg a b
    Pop  :: SProg a (SProg b c) -> SProg b c
    Push :: Int -> SProg b c -> SProg Int (SProg b c)
    Dup  :: SProg a b -> SProg a (SProg a b)
    Dup2 :: SProg a b -> SProg a (SProg b (SProg a b))
    Flip :: SProg a (SProg b c) -> SProg b (SProg a c)
    Add  :: SProg Int (SProg Int a) -> SProg Int a
    Sub  :: SProg Int (SProg Int a) -> SProg Int a
    Mul  :: SProg Int (SProg Int a) -> SProg Int a
    Leq  :: SProg Int (SProg Int a) -> SProg Bool a
    Geq  :: SProg Int (SProg Int a) -> SProg Bool a
    Not  :: SProg Bool a -> SProg Bool a
    And  :: SProg Bool (SProg Bool a) -> SProg Bool a
    Or   :: SProg Bool (SProg Bool a) -> SProg Bool a
    Seq  :: SProg a b -> SProg c d -> SProg c d
    If   :: SProg Bool a -> SProg b c -> SProg b c -> SProg b c

instance Show (SProg a b) where
    show Nil          = "Nil"
    show (Noop s)     = "Noop (" ++ (show s) ++ ")"
    show (Pop s)      = "Pop ("  ++ (show s) ++ ")"
    show (Push n s)   = "Push "  ++ (show n) ++ " (" ++ (show s) ++ ")"
    show (Dup s)      = "Dup ("  ++ (show s) ++ ")"
    show (Dup2 s)     = "Dup2 (" ++ (show s) ++ ")"
    show (Flip s)     = "Flip (" ++ (show s) ++ ")"
    show (Add s)      = "Add ("  ++ (show s) ++ ")"
    show (Sub s)      = "Sub ("  ++ (show s) ++ ")"
    show (Mul s)      = "Mul ("  ++ (show s) ++ ")"
    show (Leq s)      = "Leq ("  ++ (show s) ++ ")"
    show (Geq s)      = "Geq ("  ++ (show s) ++ ")"
    show (Not s)      = "Not ("  ++ (show s) ++ ")"
    show (And s)      = "And ("  ++ (show s) ++ ")"
    show (Or s)       = "Or ("  ++ (show s) ++ ")"
    show (Seq s1 s2)  = "Seq (" ++ (show s1) ++ ") (" ++ (show s2) ++ ")"
    show (If c s1 s2) = "If (" ++ (show c) ++ ") (" ++ (show s1) ++ ") (" ++ (show s2) ++ ")"

data Value = I Int
           | B Bool

instance Show Value where
    show (I a) = show a
    show (B b) = show b

evalAritOp :: (Int -> Int -> Int) -> SProg Int (SProg Int a) -> [Value]
evalAritOp op s =
    let xs    = eval s
        (I a) = head xs
        (I b) = head $ tail xs
    in (I $ a `op` b) : (tail $ tail xs)

evalCompOp :: (Int -> Int -> Bool) -> SProg Int (SProg Int a) -> [Value]
evalCompOp op s =
    let xs    = eval s
        (I a) = head xs
        (I b) = head $ tail xs
    in (B $ a `op` b) : (tail $ tail xs)

evalLogicOp :: (Bool -> Bool -> Bool) -> SProg Bool (SProg Bool a) -> [Value]
evalLogicOp op s =
    let xs    = eval s
        (B a) = head xs
        (B b) = head $ tail xs
    in (B $ a `op` b) : (tail $ tail xs)

eval :: SProg a b -> [Value]
eval Nil         = [I 0]
eval (Noop s)    = eval s
eval (Pop s)     = tail $ eval s
eval (Push x s)  = (I x) : (eval s)
eval (Dup s)     =
    let xs = eval s
        x  = head xs
    in x : xs
eval (Dup2 s) =
    let xs = eval s
        x  = head xs
        y  = head $ tail xs
    in x : y : xs
eval (Flip s) =
    let xs = eval s
        x  = head xs
        y  = head $ tail xs
        ys = tail $ tail xs
    in y : x : ys
eval (Add s) = evalAritOp (+) s
eval (Sub s) = evalAritOp (-) s
eval (Mul s) = evalAritOp (*) s
eval (Leq s) = evalCompOp (<=) s
eval (Geq s) = evalCompOp (>=) s
eval (Not s) =
    let xs = eval s
        (B x)  = head xs
    in (B $ not x) : (tail xs)
eval (And s) = evalLogicOp (&&) s
eval (Or s)  = evalLogicOp (||) s
eval (Seq s1 s2)  = (eval s2) ++ (eval s1)
eval (If c s1 s2) =
    let xs    = eval c
        (B b) = head xs
    in if b then eval s1 else eval s2
