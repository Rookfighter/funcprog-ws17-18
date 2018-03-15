{-#LANGUAGE GADTs, TypeOperators#-}
-- SafeStackCalc.hs
--
--     Author: Fabian Meyer
-- Created On: 27 Feb 2018

module SafeStackCalc where

data None = None
    deriving (Show)

data a <:> b = Cons a b

instance (Show a, Show b) => Show (a <:> b) where
    show (Cons a b) = show a ++ "," ++ show b

data SProg a where
    Nil   :: SProg Int
    Noop  :: SProg a -> SProg a
    Pop   :: SProg (a <:> b) -> SProg b
    Push  :: a -> SProg b -> SProg (a <:> b)
    Dup   :: SProg (a <:> b) -> SProg (a <:> (a <:> b))
    Dup2  :: SProg (a <:> (b <:> c)) -> SProg (a <:> (b <:> (a <:> (b <:> c))))
    Flip  :: SProg (a <:> (b <:> c)) -> SProg (b <:> (a <:> c))
    Add   :: SProg (Int <:> (Int <:> a)) -> SProg (Int <:> a)
    Sub   :: SProg (Int <:> (Int <:> a)) -> SProg (Int <:> a)
    Mul   :: SProg (Int <:> (Int <:> a)) -> SProg (Int <:> a)
    Div   :: SProg (Int <:> (Int <:> a)) -> SProg (Int <:> a)
    Eq    :: SProg (Int <:> (Int <:> a)) -> SProg (Bool <:> a)
    Leq   :: SProg (Int <:> (Int <:> a)) -> SProg (Bool <:> a)
    Geq   :: SProg (Int <:> (Int <:> a)) -> SProg (Bool <:> a)
    Not   :: SProg (Bool <:> a) -> SProg (Bool <:> a)
    And   :: SProg (Bool <:> (Bool <:> a)) -> SProg (Bool <:> a)
    Or    :: SProg (Bool <:> (Bool <:> a)) -> SProg (Bool <:> a)
    Seq   :: SProg a -> (SProg a -> SProg b) -> SProg b
    If    :: (SProg a -> SProg b) -> (SProg a -> SProg b) -> SProg (Bool <:> a) -> SProg b
    While :: (SProg a -> SProg (Bool <:> a)) -> SProg (Bool <:> a) -> SProg a

(+>) :: SProg a -> (SProg a -> SProg b) -> SProg b
(+>) = Seq

pushify :: SProg a -> SProg a
pushify Nil              = Nil
pushify (Noop s)         = s
pushify (Pop (Push a s)) = s
pushify (Push a s)       = Push a s
pushify (Dup (Push a s)) = Push a . Push a $ s
pushify (Dup2 (Push a (Push b s))) = Push a . Push b . Push a . Push b $ s
pushify (Flip (Push a (Push b s))) = Push b . Push a $ s
pushify (Add (Push a (Push b s)))  = Push (a+b) s
pushify (Sub (Push a (Push b s)))  = Push (a-b) s
pushify (Mul (Push a (Push b s)))  = Push (a*b) s
pushify (Div (Push a (Push b s)))  = Push (a `div` b) s
pushify (Eq  (Push a (Push b s)))  = Push (a == b) s
pushify (Leq (Push a (Push b s)))  = Push (a <= b) s
pushify (Geq (Push a (Push b s)))  = Push (a >= b) s
pushify (Not (Push a s))           = Push (not a) s
pushify (And (Push a (Push b s)))  = Push (a && b) s
pushify (Or (Push a (Push b s)))  = Push (a || b) s
pushify (Seq s f)    = pushify . f . pushify $ s
pushify (If f1 f2 (Push c s)) | c         = pushify $ s +> f1
                              | otherwise = pushify $ s +> f2
pushify (While f (Push c s)) | c         = pushify $ s +> f +> (While f)
                             | otherwise = s
pushify _ = undefined

eval :: SProg a -> a
eval s = evalHelp . pushify $ s

evalHelp :: SProg a -> a
evalHelp Nil = 0
evalHelp (Push a s) = Cons a $ evalHelp s

-- a b
-- b a
-- b a b a
-- a b b a
-- a/b b a
-- a/b*b a
-- a a/b*b
-- a-a/b*b

smod :: SProg (Int <:> (Int <:> a)) -> SProg (Int <:> a)
smod s = s +> Flip +> Dup2 +> Flip +> Div +> Mul +> Flip +> Sub

-- wmod :: SProg (Int <:> Int <:> a) -> SProg (Int <:> a)
-- wmod s = s +> Dup2 +> Pop +> (Push 0) +> modwhile
--     where modwhile s1 = s1 +> modop +> cond +> While (\_ -> s1 +>)
--           body s1 = s1 +> (Push 1) +> Add +> cond
--           modop s1 = s1 +> Mul +> Sub
--           cond s1 = s1 +> Dup2 +> Geq
--
--     Pop . Flip . modop $ While (\s1 -> Geq (modop s1)) (\s1 -> inc s1) (Push 0 (Pop (Dup2 s)))
--     where modop s1 = Sub (Flip (Mul s1))


myfoo = Nil +> (Push 1) +> (Push 3) +> Add +> (Push 5) +> Leq
