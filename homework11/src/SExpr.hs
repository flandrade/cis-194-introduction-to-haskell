----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 11
--
----------------------------------------------------------------------

module SExpr where

import AParser

-- base
import Control.Applicative
import Data.Char


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- >>> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
-- Just ("","abcdeFGh")

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- |
--
-- >>> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- >>> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
-- Nothing

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace


-- |
--
-- >>> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- >>> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- >>> runParser ident "2bad"
-- Nothing
-- >>> runParser ident ""
-- Nothing

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

type Ident =
  String


data Atom
  = N Integer
  | I Ident
  deriving Show


data SExpr
  = A Atom
  | Comb [SExpr]
  deriving Show


-- |
--
-- >>> runParser parseSExpr "  5  "
-- Just (A (N 5),"")
-- >>> runParser parseSExpr "foo3"
-- Just (A (I "foo3"),"")
-- >>> runParser parseSExpr "((lambda x x) 3)"
-- Just (Comb [Comb [A (I "lambda"),A (I "x"),A (I "x")],A (N 3)],"")
-- >>> runParser parseSExpr "(lambda x x) 3"
-- Just (Comb [A (I "lambda"),A (I "x"),A (I "x")],"3")
-- >>> runParser parseSExpr "(lambda x x"
-- Nothing

parseSExpr :: Parser SExpr
parseSExpr = undefined
