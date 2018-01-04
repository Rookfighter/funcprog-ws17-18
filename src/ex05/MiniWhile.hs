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
