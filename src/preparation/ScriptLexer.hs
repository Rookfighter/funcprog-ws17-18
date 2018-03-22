-- ScriptLexer.hs
--
--     Author: Fabian Meyer
-- Created On: 22 Mar 2018

module ScriptLexer (Token(..), lexScript) where

import ApplicativeParser
import Data.Char

data Token = TIdent String
           | TAssign
           | TSep
           | TInt Int
           | TBool Bool
           | TLOp String
           | TAOp String
           | TPar Char
           | TKeyword String
    deriving (Show)

type Tokenizer = Parser Char Token
type ScriptLexer = Parser Char [Token]

lexScript :: String -> Maybe [Token]
lexScript s = parse lexer s

lexer :: ScriptLexer
lexer = (some (
    spaces *>
    (assignToken
    <|> sepToken
    <|> intToken
    <|> boolToken
    <|> lopToken
    <|> aopToken
    <|> parToken
    <|> keywordToken
    <|> identToken)) <* spaces)

spaces :: Parser Char [Char]
spaces = many $ satisfy isSpace

identToken :: Tokenizer
identToken =
    fmap TIdent $ (:)
    <$> satisfy isStartIdent
    <*> many (satisfy isIdent)
    where isIdent c = isAlphaNum c || c == '_'
          isStartIdent c = isAlpha c || c == '_'

assignToken :: Tokenizer
assignToken = TAssign <$ lit '='

sepToken :: Tokenizer
sepToken = TSep <$ lit ';'

intToken :: Tokenizer
intToken = TInt . read <$> some (satisfy isDigit)

boolToken :: Tokenizer
boolToken = TBool . toBool <$> (word "false" <|> word "true")
    where toBool "true" = True
          toBool "false" = False
          toBool _ = undefined

lopToken :: Tokenizer
lopToken = TLOp
    <$> (word ">"
     <|> word "<"
     <|> word ">="
     <|> word "<="
     <|> word "=="
     <|> word "~=")

aopToken :: Tokenizer
aopToken = TAOp
    <$> (word "+"
     <|> word "-"
     <|> word "*"
     <|> word "/")

parToken :: Tokenizer
parToken = TPar
    <$> (lit '('
     <|> lit ')')

keywordToken :: Tokenizer
keywordToken = TKeyword <$> (
        word "while"
    <|> word "do"
    <|> word "done"
    <|> word "if"
    <|> word "else")
