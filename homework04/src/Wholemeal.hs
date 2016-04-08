----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 04
--
----------------------------------------------------------------------

module Wholemeal where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

-- |
--
-- >>> fun1 [1,3,5,7] == fun1' [1,3,5,7]
-- True
-- >>> fun1 [1,2,3] /= fun1' [1,2,3]
-- False
-- >>> fun2 10 == fun2' 10
-- True
-- >>> fun2 15 /= fun2' 15
-- False

fun1' :: [Integer] -> Integer
fun1' =  product . map (2-) . filter even

-- (alternative solution)
--fun1' =  foldl (\x acc -> x * (acc - 2)) 1 . filter even 

-- Check if n is even
isCase :: Integer -> Integer
isCase n = if even n then div n 2 else 3 * n + 1

-- Sum only even numbers
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate isCase


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

data Tree a =
    Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- Insert first element
singleElem :: Integer -> a -> Tree a
singleElem n x = Node n Leaf x Leaf

-- Calculate the height of a single tree
treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node _ left _ right) = 1 + max (treeHeight left) (treeHeight right)

-- Insert an element in the tree
treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = singleElem 0 x
treeInsert x (Node level Leaf a Leaf) = Node (level+1) (treeInsert x Leaf) a Leaf
treeInsert x (Node level left a right)
  | treeHeight left == treeHeight right = Node (treeHeight insertLeft + 1) insertLeft a right
  | treeHeight left > treeHeight right  = Node level left a insertRight
  | otherwise                               = Node level insertLeft a right
  where insertLeft  = treeInsert x left
        insertRight = treeInsert x right

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf



----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False

-- Filter False and applies definition => a xor b = a /= b
xor :: [Bool] -> Bool
xor = foldl (/=) False . filter id

-- |
--
-- >>> map' (+1) [1,2,3]
-- [2,3,4]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x a -> f x : a) []

-- Optional

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = undefined

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [x*2 + 1 | x <- [1..(2*n + 1)]]
