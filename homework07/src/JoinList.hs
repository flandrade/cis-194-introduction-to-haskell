----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------

module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- let x = Single (Product 2) 'j'
-- let y = Single (Product 7) 'a'

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) listRight listLeft = Append mTop listRight listLeft
  where mTop = tag listRight `mappend` tag listLeft

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = undefined

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------
