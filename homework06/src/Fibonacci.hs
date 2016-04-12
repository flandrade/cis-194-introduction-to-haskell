----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 06
--
----------------------------------------------------------------------

module Fibonacci where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib i | i <- [0..]]


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- Binet's Formula
fib2 :: Integer -> Integer
fib2 n = round (phi ** fromIntegral n / s)
  where
    s = sqrt 5 :: Double
    phi = (1 + s) / 2

fibs2 :: [Integer]
fibs2 = [fib2 i | i <- [0..]]

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

data Stream n = Cons n (Stream n)

streamToList :: Stream a -> [a]
streamToList (Cons n st) = n : streamToList st

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList 


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x sti) = Cons (f x) (streamMap f sti)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed r n = Cons n (streamFromSeed r x)
  where x = r n


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

--ruler :: Stream Integer
--ruler = streamFromSeed rulerSerie 1

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

--x :: Stream Integer
--x = undefinedn


----------------------------------------------------------------------
-- Exercise 7 (Optional)
----------------------------------------------------------------------

--fib4 :: Integer -> Integer
--fib4 = undefined
