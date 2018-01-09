-- MiniWhile.hs
--
--     Author: Fabian Meyer
-- Created on: 04 Jan 2018

module MiniWhile where

import Data.Char
import Data.List
import ParserCon

-- ========================================
--                  Lexer
-- ========================================

data Token = TSep -- ';'
           | TAsgn -- ':='
           | TNum Integer -- numbers
           | TId Id -- identifieres
           | TOp Op -- operators
           | TCmp Cmp -- comparison
           | TKw String -- keywords
           | TPar Char
    deriving (Eq, Show)

-- lex separator
lTSep :: Parser Char Token
lTSep = TSep <$ lit ';'

-- lex assign symbol
lTAsgn :: Parser Char Token
lTAsgn = TAsgn <$ litStr ":="

-- lex numbers
lTNum :: Parser Char Token
lTNum = TNum . read <$> many1 (satisfy isDigit)

-- lex identifiers
lTId :: Parser Char Token
lTId = fmap TId $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- lex arithmetic operators
lTOp :: Parser Char Token
lTOp = TOp
    <$> (litStr "+"
    <|> litStr "-"
    <|> litStr "*"
    <|> litStr "/")

-- lex comparison operators
lTCmp :: Parser Char Token
lTCmp = TCmp
    <$> (litStr "<="
    <|> litStr ">"
    <|> litStr "=="
    <|> litStr "!=")

-- lex keywords
lTKw :: Parser Char Token
lTKw = TKw
    <$> (litStr "while"
    <|> litStr "done"
    <|> litStr "do"
    <|> litStr "if"
    <|> litStr "then"
    <|> litStr "else"
    <|> litStr "fi"
    <|> litStr "not")

-- lex parethesis
lTPar :: Parser Char Token
lTPar = TPar
    <$> (lit '('
    <|> lit ')')

-- lex tokens
lToken :: Parser Char Token
lToken =
    lTKw
    <|> lTId
    <|> lTSep
    <|> lTAsgn
    <|> lTNum
    <|> lTCmp
    <|> lTOp
    <|> lTPar

-- run lexer
lexer :: String -> Maybe [Token]
lexer = parse $ many1 (pSkipSpace *> lToken) <* pSkipSpace

pSkipSpace :: Parser Char String
pSkipSpace = many (satisfy isSpace)

-- ==================================
--              Parsing
-- ==================================

data Program = Program [Stmt]
    deriving (Show, Eq)
data Stmt = While Exp [Stmt]
          | Asgn AExp Exp
    deriving (Show, Eq)
data Exp = If Exp Exp Exp
         | Cmp AExp Cmp AExp
         | Inv Exp
         | AExp AExp
    deriving (Show, Eq)
data AExp = Num Integer
          | Var Id
          | Op AExp Op AExp
    deriving (Show, Eq)
type Id = String
type Op = String
type Cmp = String

-- parse a variable
pVar :: Parser Token AExp
pVar = Var <$> try g
    where g (TId s) = Just s
          g _       = Nothing

-- parse a number
pNum :: Parser Token AExp
pNum = Num <$> try g
    where g (TNum n) = Just n
          g _        = Nothing

-- parse an operation
pOp :: Parser Token AExp
pOp =
    pure Op
        <*  (lit $ TPar '(')
        <*> pAExp
        <*> try g
        <*> pAExp
        <*  (lit $ TPar ')')
    where g (TOp s) = Just s
          g _       = Nothing

-- parse an algebraic expression
pAExp :: Parser Token AExp
pAExp = pVar
    <|> pNum
    <|> pOp

-- parse an if expression
pIf :: Parser Token Exp
pIf =
    pure If
        <*  (lit $ TKw "if")
        <*> pExp
        <*  (lit $ TKw "then")
        <*> pExp
        <*  (lit $ TKw "else")
        <*> pExp
        <*  (lit $ TKw "fi")

-- parse comparison
pCmp :: Parser Token Exp
pCmp =
    pure Cmp
        <*> pAExp
        <*> try g
        <*> pAExp
    where g (TCmp s) = Just s
          g _        = Nothing

-- parse inversion
pInv :: Parser Token Exp
pInv =
    pure Inv
        <*  (lit $ TKw "not")
        <*> pExp

-- parse an expression
pExp :: Parser Token Exp
pExp = pIf
    <|> pCmp
    <|> pInv
    <|> (AExp <$> pAExp)

-- parse an assignment
pAsgn :: Parser Token Stmt
pAsgn =
    pure Asgn
         <*> pVar
         <*  (lit $ TAsgn)
         <*> pExp

-- parse a while statement
pWhile :: Parser Token Stmt
pWhile =
    pure While
        <*  (lit $ TKw "while")
        <*> pExp
        <*  (lit $ TKw "do")
        <*> pStmts
        <*  (lit $ TKw "done")

-- parse a statement
pStmt :: Parser Token Stmt
pStmt =
    pWhile
    <|>
    pAsgn

-- parse multiple statements
pStmts :: Parser Token [Stmt]
pStmts =
    pure (:)
        <*> pStmt
        <*  (lit $ TSep)
        <*> pStmts
    <|>
    pure return
        <*> pStmt

-- run parser
parser :: Parser Token Program
parser = Program <$> pStmts

parseString :: String -> Maybe Program
parseString s = do
    l <- lexer s
    parse parser l

-- ^ Utilities
litStr = foldr (\a b -> (:) <$> a <*> b) (pure []) . map lit
many1 p = (:) <$> p <*> many p
