----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------

module JoinList where

import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- let x = Single (Product 2) 'j'
-- let y = Single (Product 7) 'a'
-- let b = (+++) (Single (Product 3) 'x') (Append (Product {getProduct = 14}) (Single (Product {getProduct = 2}) 'j') (Single (Product {getProduct = 7}) 'a'))
-- let b = Append (Product 210) (Append (Product 30) (Single (Product 5) 'y') (Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) (Single (Product 7) 'h')

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) listRight listLeft = Append mTop listRight listLeft
  where mTop = tag listRight `mappend` tag listLeft

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------


getSized :: Sized b => b -> Int
getSized = getSize . size

getWrappedSize :: (Sized b, Monoid b) => JoinList b a -> Int
getWrappedSize = getSized . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ x (Single _ value)
  | x == 0                      = Just value
  | otherwise                   = Nothing
indexJ x (Append s listLeft listRight)
  | x > getSized s              = Nothing
  | x < getWrappedSize listLeft = indexJ x listLeft
  | otherwise                   = indexJ (x - getWrappedSize listLeft) listRight

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ x ls@(Single _ _)
  | x == 0                      = ls
  | otherwise                   = Empty
dropJ x (Append s listLeft listRight)
  | x > (getSized s)            = Empty
  | x < getWrappedSize listLeft = dropJ x listLeft
  | otherwise                   = dropJ (x - getWrappedSize listLeft) listRight

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ x ls@(Single _ _)
  | x == 0                      = Empty
  | otherwise                   = ls
takeJ x ls@(Append s listLeft listRight)
  | x > (getSized s)            = ls
  | x < getWrappedSize listLeft = takeJ x listLeft
  | otherwise                   = (+++) listLeft $ dropJ (x - getWrappedSize listLeft) listRight


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------
