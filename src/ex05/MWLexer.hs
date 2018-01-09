-- MWLexer.hs
--
--     Author: Fabian Meyer
-- Created On: 09 Jan 2018

module MWLexer where

import Data.Char
import Data.List
import ParserCon

type Id = String
type Op = String
type Cmp = String

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

-- ^ Utilities

litStr = foldr (\a b -> (:) <$> a <*> b) (pure []) . map lit
many1 p = (:) <$> p <*> many p
