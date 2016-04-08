{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 05
--
----------------------------------------------------------------------

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
-- True

eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add firsTerm secondTerm) = (eval firsTerm) + (eval secondTerm)
eval (ExprT.Mul firsTerm secondTerm) = (eval firsTerm) * (eval secondTerm)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr x = case parseExp ExprT.Lit ExprT.Add ExprT.Mul x of Nothing -> Nothing
                                                             Just ex -> Just (eval ex)


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

class Expr a where
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  add a b = (ExprT.Add a b)
  mul a b = (ExprT.Mul a b)
  lit a   = (ExprT.Lit a)

reify :: ExprT -> ExprT
reify = id


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit a   = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit a = (if a <= 0 then False else True)
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit a = Mod7 a
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod (a - b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


----------------------------------------------------------------------
-- Exercise 5 (do this OR exercise 6)
----------------------------------------------------------------------

--compile :: String -> Maybe Program
--compile = undefined


----------------------------------------------------------------------
-- Exercise 6 (do this OR exercise 5)
----------------------------------------------------------------------

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

-- |
--
-- >>> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54
