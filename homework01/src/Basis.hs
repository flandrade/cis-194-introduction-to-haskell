----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 01
--
----------------------------------------------------------------------

module Basis where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []

-- Compute the remainder when a number is divided by 10
getRem :: Integer -> Integer
getRem n = n `rem` 10

-- Compute fractional division by 10
getDiv :: Integer -> Integer
getDiv n = n `div` 10

-- Get a reverse list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = getRem n : toDigitsRev (getDiv n)

-- Get list of digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]

-- Compute the length of a list
lengthList :: [Integer] -> Integer
lengthList []    = 0
lengthList (_:x) = 1 + lengthList x

-- double every two numbers of a list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []              = []
doubleEveryOther [x]             = [x]
doubleEveryOther (x:y:zs)
  | even . lengthList $ (x:y:zs) =  2 * x : y: doubleEveryOther zs
  | otherwise                    =  x : 2 * y : doubleEveryOther zs

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> sumDigits [16,7,12,5]
-- 22


-- Sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits [x]     = x
sumDigits (x:y:z) = sum (toDigits x) + sum (toDigits y) + sumDigits z

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False

-- Validate a credit card number
validate :: Integer -> Bool
validate n
  | (getRem . sumDigits . doubleEveryOther . toDigits $ n) == 0 = True
  | otherwise                                                   = False

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg)

-- |
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

-- Recursive solution
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1     = [(a,c)]
  | otherwise = hanoi (n-1) a b c ++ hanoi 1 a c b  ++ hanoi (n-1) c a b

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' = undefined
