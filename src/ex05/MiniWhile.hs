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
           | TNum Integer -- numbers
           | TId Id -- identifieres
           | TOp String -- operators
           | TCmp String -- comparison
           | TKw String -- keywords
    deriving (Eq, Show)

pTSep = TSep <$ lit ';'
pTAsgn = TAsgn <$ litStr ":="
pTNum = TNum . read <$> many1 (satisfy isDigit)
pTId = fmap TId $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
pTOp = TOp
    <$> (litStr "+"
    <|> litStr "-"
    <|> litStr "*"
    <|> litStr "/")
pTCmp = TCmp
    <$> (litStr "<="
    <|> litStr ">"
    <|> litStr "=="
    <|> litStr "!=")
pTKw = TKw
    <$> (litStr "while"
    <|> litStr "done"
    <|> litStr "do"
    <|> litStr "if"
    <|> litStr "then"
    <|> litStr "else"
    <|> litStr "fi")

pToken :: Parser Char Token
pToken =
    pTKw
    <|> pTId
    <|> pTSep
    <|> pTAsgn
    <|> pTNum
    <|> pTCmp
    <|> pTOp

lexer :: String -> Maybe [Token]
lexer = parse $ many1 (pSkipSpace *> pToken) <* pSkipSpace

pSkipSpace :: Parser Char String
pSkipSpace = many (satisfy isSpace)

-- ^ Utilities
litStr = foldr (\a b -> (:) <$> a <*> b) (pure []) . map lit
many1 p = (:) <$> p <*> many p
