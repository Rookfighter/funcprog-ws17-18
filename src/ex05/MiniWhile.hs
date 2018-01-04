module MiniWhile where

import Data.Char
import Data.List
import ParserCon

-- ^ Parsing

data Program = Program [Stmt]
    deriving (Show, Eq)
data Stmt = Asgn Id Exp
            -- Add more things here
    deriving (Show, Eq)
data Exp = Num Integer
         | Var Id
         -- Add more things here
    deriving (Show, Eq)
type Id = String

-- recognizes one or more elements
psome :: Parser t r -> Parser t [r]
psome p = (:) <$> p <*> pmany p

-- alias for psome (according to exercise)
pmany1 = psome

-- recognizes zero or more elements
pmany :: Parser t r -> Parser t [r]
pmany p = psome p <|> pure []

-- reads an Integer
pReadInt :: Parser Char Integer
pReadInt = read <$> pmany1 (satisfy isDigit)

-- reads a comma separated list of Integers
pReadIntList :: Parser Char [Integer]
pReadIntList =
    -- first alt: read a comma separated list of ints
    pure (:)
        <*> pReadInt -- read a number
        <*  lit ',' -- followed by a comma
        <*  pmany (satisfy isSpace) -- followed by arbitrary spaces
        <*> pReadIntList
    <|>
    -- second alt: read only one int, return it as list
    pure return
        <*> pReadInt -- just read a number
    <|>
    -- everything else recognizes an empty list
    pure []

-- reads a haskell style list of Integers
pIntList :: Parser Char [Integer]
pIntList =
    pure id
    <*  lit '['
    <*> pReadIntList
    <*  lit ']'

-- accepts any kind of palindromes
pPali :: (Eq r) => Parser t r -> Parser t [r]
pPali p =
    (pmany1 $ p)
        >>= \r ->
            if r == reverse r then return r else empty

-- accepts palindromes of 'a' and 'b'
pPaliAB :: Parser Char String
pPaliAB = pPali (lit 'a' <|> lit 'b')

pTwice :: (Eq t) => Parser t [t] -> Parser t [t]
pTwice p = p >>= \r -> return (r ++ r)

parseString :: String -> Maybe Program
parseString s = do
    l <- lexer s
    parse parser l

-- TODO implement
parser :: Parser Token Program
parser = undefined

-- ^ Lexing
-- Use this lexer to tokenize the input before parsing

data Token = TSep -- ';'
           | TAsgn -- ':='
           | TNum Integer
           | TId Id
           -- Add more things here
    deriving (Eq, Show)

lexer :: String -> Maybe [Token]
lexer = parse $ many1 (skipSpace *> p_tok) <* skipSpace

skipSpace = many (satisfy isSpace)
p_tok =
    t_alnum
    <|> t_sep
    <|> t_asgn
    <|> t_num

t_num = TNum . read <$> many1 (satisfy isDigit)
t_sep = TSep <$ lit ';'
t_asgn = TAsgn <$ string ":="
t_alnum = fmap mkToken $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
    where mkToken i        = TId i

-- ^ Utilities
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs
many1 p = (:) <$> p <*> many p
