-- ApplicativeParser.hs
--
--     Author: Fabian Meyer
-- Created On: 22 Mar 2018

module ApplicativeParser (
    module Control.Applicative,
    module Control.Monad,
    Parser,
    satisfy,
    msatisfy,
    lit,
    word,
    parse
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe

-- A ParserF takes a list of tokens, parses them and returns a list of possible
-- results with a list of remaining tokens.
type ParserF t r = [t] -> [(r,[t])]

-- Recognizes the empty language.
pemptyF :: ParserF t r
pemptyF _ = []

-- Recognizes the empty word. Returns given result without consuming any token.
succeedF :: r -> ParserF t r
succeedF r ts = [(r, ts)]

-- Accepts the very first token if it satisfies the given predicate.
satisfyF :: (t -> Bool) -> ParserF t t
satisfyF p [] = []
satisfyF p (t:ts) | p t       = [(t,ts)]
                  | otherwise = []

-- Accepts and transforms the very first token if the given function returns
-- something.
msatisfyF :: (t -> Maybe r) -> ParserF t r
msatisfyF f [] = []
msatisfyF f (t:ts) = case f t of
    Just r  -> [(r,ts)]
    Nothing -> []

litF :: Eq t => t -> ParserF t t
litF t = satisfyF (==t)

-- Creates alternative between two parsers.
-- Returns sum of their possible parsing results.
paltF :: ParserF t r -> ParserF t r -> ParserF t r
paltF p1 p2 ts = (p1 ts) ++ (p2 ts)

-- Creates sequence of two parsers.
-- Runs first parser on tokens and then runs second parser on the remaining
-- tokens.
pseqF :: ParserF t (r -> s) -> ParserF t r -> ParserF t s
pseqF p1 p2 ts = concat . map runP2 $ p1 ts
    where runP2 (f, ts1) = pmapF f p2 $ ts1

-- Maps the results of a parser to a different domain
pmapF :: (r -> s) -> ParserF t r -> ParserF t s
pmapF f p1 ts = map transRet $ p1 ts
    where transRet (r, ts2) = (f r, ts2)

newtype Parser t r = Parser {runParser :: ParserF t r}

instance Functor (Parser t) where
    fmap f (Parser p) = Parser (pmapF f p)

instance Applicative (Parser t) where
    pure r = Parser $ succeedF r
    (Parser p1) <*> (Parser p2) = Parser (pseqF p1 p2)

-- Alternative also defines some and many.
instance Alternative (Parser t) where
    empty = Parser (pemptyF)
    (Parser p1) <|> (Parser p2) = Parser (paltF p1 p2)

instance Monad (Parser t) where
    return = pure
    (Parser p1) >>= f = Parser (\ts -> concat . map runP2 $ p1 ts)
        where runP2 (r, ts1) = runParser (f r) ts1

-- Wrapped version of satisfyF.
satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser (satisfyF p)

-- Wrapped version of msatisfyF.
msatisfy :: (t -> Maybe r) -> Parser t r
msatisfy f = Parser (msatisfyF f)

-- Wrapped version of litF.
lit :: Eq t => t -> Parser t t
lit t = Parser (litF t)

word :: Eq t => [t] -> Parser t [t]
word ts = foldr (\t p-> (:) <$> lit t <*> p) (pure []) ts

parse :: Parser t r -> [t] -> Maybe r
parse p ts = fmap fst . listToMaybe . filter hasNoTokens $ parseAll p ts
    where hasNoTokens (r, ts1) = null ts1

parseAll :: Parser t r -> [t] -> [(r,[t])]
parseAll (Parser p) ts = p ts
