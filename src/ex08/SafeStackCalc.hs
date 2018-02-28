{-#LANGUAGE GADTs#-}
-- SafeStackCalc.hs
--
--     Author: Fabian Meyer
-- Created On: 27 Feb 2018

module SafeStackCalc where

data Empty;
data NonEmpty;
data None;

data SProg a b c where
    Nil  :: SProg Int None Empty
    Noop :: SProg a b c -> SProg a b c
    Pop  :: SProg a (SProg b c d) NonEmpty -> SProg b c d
    Push :: Int -> SProg b c d -> SProg Int (SProg b c d) NonEmpty
    Dup  :: SProg a b c -> SProg a (SProg a b c) NonEmpty
    Dup2 :: SProg a b c -> SProg a (SProg b (SProg a b c) NonEmpty) NonEmpty
    Flip :: SProg a (SProg b c d) NonEmpty -> SProg b (SProg a c d) NonEmpty
    Add  :: SProg Int (SProg Int a b) NonEmpty -> SProg Int a NonEmpty
    Sub  :: SProg Int (SProg Int a b) NonEmpty -> SProg Int a NonEmpty
    Mul  :: SProg Int (SProg Int a b) NonEmpty -> SProg Int a NonEmpty
    Leq  :: SProg Int (SProg Int a b) NonEmpty -> SProg Bool a NonEmpty
    Geq  :: SProg Int (SProg Int a b) NonEmpty -> SProg Bool a NonEmpty
    Not  :: SProg Bool a NonEmpty -> SProg Bool a NonEmpty
    And  :: SProg Bool (SProg Bool a b) NonEmpty -> SProg Bool a NonEmpty
    Or   :: SProg Bool (SProg Bool a b) NonEmpty -> SProg Bool a NonEmpty
    Seq  :: SProg a b c -> SProg d e f -> SProg d e f
    If   :: SProg Bool a NonEmpty -> SProg b c d -> SProg b c d -> SProg b c d

instance Show (SProg a b c) where
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

evalAritOp :: (Int -> Int -> Int) -> SProg Int (SProg Int a b) NonEmpty -> [Value]
evalAritOp op s =
    let xs    = eval s
        (I a) = head xs
        (I b) = head $ tail xs
    in (I $ a `op` b) : (tail $ tail xs)

evalCompOp :: (Int -> Int -> Bool) -> SProg Int (SProg Int a b) NonEmpty -> [Value]
evalCompOp op s =
    let xs    = eval s
        (I a) = head xs
        (I b) = head $ tail xs
    in (B $ a `op` b) : (tail $ tail xs)

evalLogicOp :: (Bool -> Bool -> Bool) -> SProg Bool (SProg Bool a b) NonEmpty -> [Value]
evalLogicOp op s =
    let xs    = eval s
        (B a) = head xs
        (B b) = head $ tail xs
    in (B $ a `op` b) : (tail $ tail xs)

eval :: SProg a b c -> [Value]
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
