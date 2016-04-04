----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------

module Golf where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True, False]
-- [[True,False],[False]]
-- >>> skips []
-- []

-- Get the nth element of a list (y:ys) by dropping (n-1) elements of the
-- list ys and taking the first element y.
-- It repeats until the list y:ys is empty.
-- getAllN 2 "hola" = "oa"
getAllN :: Int -> [a] -> [a]
getAllN n x = case drop (n-1) x of (y:ys) -> y : getAllN n ys
                                   [] -> []

-- The nth list in the output contains every nth element from the input list.
-- It applies the getAllN function to the list and n = [1..length of list]
-- The length of the output list is the same as the imput.
skips :: [a] -> [[a]]
skips x = [getAllN i x | i <- [1..length x]]


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []

-- Checks list (x:y:z:xs): if x>y and y<z in order to include element in output
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > x && y > z = y : localMaxima (y:z:xs)
    | otherwise      = localMaxima (y:z:xs)
localMaxima _ = []


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"


-- Get the frequency of a number in a list
getFreq :: Integer -> [Integer] -> Int
getFreq n = length . filter (==n)

-- Get the list of frequencies from 0 to 9
-- getAllFreq [1,1,1,5] = [0,3,0,0,0,1,0,0,0,0]
getAllFreq :: [Integer] -> [Int]
getAllFreq x = [getFreq i x | i <- [0..9]]

-- Print * according to frequencies and the level of histogram  'n'
-- printStar [0,3,0,0,0,1,0,0,0,0] 1 = " *   *    \n"
printStar :: [Int] -> Int -> String
printStar [] _ = "\n"
printStar (x:xs) n
    | x >= n    = "*" ++ printStar xs n
    | otherwise = " "  ++ printStar xs n

-- Concatenate levels of histogram and "==========\n0123456789\n"
histogram :: [Integer] -> String
histogram x = concatMap (printStar y . fromIntegral) (reverse [1 .. maximum y]) ++ "==========\n0123456789\n"
  where y = getAllFreq x
