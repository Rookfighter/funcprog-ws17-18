-- MWParser.hs
--
--     Author: Fabian Meyer
-- Created On: 09 Jan 2018

module MWParser where

import Data.Char
import Data.List
import ParserCon
import MWLexer

data Program = Program [Stmt]
    deriving (Show, Eq)
data Stmt = While Exp [Stmt]
          | Asgn AExp Exp
          | Print Exp
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

-- parse a print statement
pPrint :: Parser Token Stmt
pPrint =
    pure Print
        <*  (lit $ TKw "print")
        <*> pExp

-- parse a statement
pStmt :: Parser Token Stmt
pStmt =
    pWhile
    <|>
    pAsgn
    <|>
    pPrint

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

parseFile :: String -> IO (Maybe Program)
parseFile f = do
    s <- readFile f
    return . parseString $ s
