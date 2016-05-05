----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Editor
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

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
indexJ _ Empty                  = Nothing
indexJ x (Single _ v)
  | x == 0                      = Just v
  | otherwise                   = Nothing
indexJ x (Append s listLeft listRight)
  | x > getSized s              = Nothing
  | x < getWrappedSize listLeft = indexJ x listLeft
  | otherwise                   = indexJ (x - getWrappedSize listLeft) listRight

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                   = Empty
dropJ x ls@(Single _ _)
  | x == 0                      = ls
  | otherwise                   = Empty
dropJ x (Append s listLeft listRight)
  | x > getSized s              = Empty
  | x < getWrappedSize listLeft = dropJ x listLeft
  | otherwise                   = dropJ (x - getWrappedSize listLeft) listRight

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                   = Empty
takeJ x ls@(Single _ _)
  | x == 0                      = Empty
  | otherwise                   = ls
takeJ x ls@(Append s listLeft listRight)
  | x > getSized s              = ls
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

instance Buffer (JoinList (Score, Size) String) where
  toString Empty        = ""
  toString (Single _ s) = s
  toString (Append _ listLeft listRight) = toString listLeft ++ toString listRight

  fromString "" = Empty
  fromString s  = foldl (+++) Empty (map f $ lines s)
    where f x = Single (scoreString x, Size 1) x

  line = indexJ

  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n + 1) b

  numLines Empty              = 0
  numLines (Single _ _)       = 1
  numLines (Append (_,s) _ _) = getSize s

  value Empty = 0
  value (Single (s,_) _)       = getScore s
  value (Append (s,_) _ _) = getScore s

-- Run Editor
main = runEditor editor (fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String)
