-- ScriptParser.hs
--
--     Author: Fabian Meyer
-- Created On: 22 Mar 2018

module ScriptParser (parseScript) where

import ApplicativeParser
import ScriptLexer

type ScriptParser = Parser Token Program

type Var = String
type Op = String

newtype Program = Program [Statement]
    deriving (Show)

data Statement = Assign Var Expr
    deriving (Show)

data Expr = EArit AritExpr
          | ELogic LogicExpr
    deriving (Show)

data LogicExpr = LConst Bool
               | LOp AritExpr Op AritExpr
    deriving (Show)

data AritExpr = AConst Int
              | AOp AritExpr Op AritExpr
    deriving (Show)

parseScript :: String -> Maybe Program
parseScript s = do
    ts <- lexScript s
    parse programParser ts

programParser :: Parser Token Program
programParser = Program <$> stmtsParser

stmtsParser :: Parser Token [Statement]
stmtsParser = return <$> stmtParser <* satisfy matchSep
    where matchSep TSep = True
          matchSep _    = False

stmtParser :: Parser Token Statement
stmtParser = pure Assign <*> msatisfy matchVar <* satisfy matchAssign <*> exprParser
    where matchVar (TIdent s) = Just s
          matchVar _          = Nothing
          matchAssign TAssign = True
          matchAssign _       = False

exprParser :: Parser Token Expr
exprParser = (ELogic <$> lexprParser)
    <|> (EArit <$> aexprParser)

lexprParser :: Parser Token LogicExpr
lexprParser = (LConst <$> msatisfy matchBool)
    <|> (LOp <$> (parenOpen *> aexprParser) <*> msatisfy matchOp <*> (aexprParser <* parenClose))
    where matchBool (TBool b) = Just b
          matchBool _         = Nothing
          matchOp (TLOp s)    = Just s
          matchOp _           = Nothing
          --    <|> (parenOpen *> lexprParser <* parenClose)

aexprParser :: Parser Token AritExpr
aexprParser = (AConst <$> msatisfy matchInt)
    <|> (AOp <$> (parenOpen *> aexprParser) <*> msatisfy matchOp <*> (aexprParser <* parenClose))
    where matchInt (TInt n)   = Just n
          matchInt _          = Nothing
          matchOp (TAOp s)    = Just s
          matchOp _           = Nothing
          --    <|> ( aexprParser )

parenOpen = satisfy (\t -> case t of (TPar '(') -> True; _ -> False)
parenClose = satisfy (\t -> case t of (TPar ')') -> True; _ -> False)
