module StackCalc (
    pop,
    dup,
    add,
    sub,
    mul,
    neg,
    push,
    readCommand
) where

-- remove the last element from the stack
pop  :: [Int] -> [Int]
pop [] = []
pop s  = init s

-- duplicate last element of the stack
dup  :: [Int] -> [Int]
dup [] = [0]
dup s  = push s (last s)

-- add the last two elements of the stack
add  :: [Int] -> [Int]
add []  = [0]
add [x] = [x]
add s   = push (pop (pop s)) ((last s) + (last (pop s)))

-- subtract second last element from last element
sub  :: [Int] -> [Int]
sub [] = [0]
sub s  = add (push (neg (pop s)) (last s))

-- multiply last two elements of the stack
mul  :: [Int] -> [Int]
mul []  = [0]
mul [x] = [0]
mul s   = push (pop (pop s)) ((last s) * (last (pop s)))

-- negate the last element of the stack
neg  :: [Int] -> [Int]
neg [] = [0]
neg s  = push (pop s) (-(last s))

-- push the given number on the stack
push :: [Int] -> Int -> [Int]
push s x = s ++ [x]

-- read text commands
readCommand :: String -> [Int] -> [Int]
readCommand "pop" s = pop s
readCommand "dup" s = dup s
readCommand "add" s = add s
readCommand "sub" s = sub s
readCommand "mul" s = mul s
readCommand "neg" s = neg s
readCommand ('p':'u':'s':'h':' ':num) s = push s (read num :: Int)
