----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
--
----------------------------------------------------------------------

module Party where

import Employee
import Data.Tree

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- glCons (Emp { empName = "Rodrigo", empFun = 10}) (GL [Emp { empName = "A", empFun = 1},Emp { empName = "B", empFun = 2}] 3)
-- moreFun (Emp { empName = "A", empFun = 10}) (Emp { empName = "X", empFun = 20})

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) = GL (employees ++ [employee]) (fun + empFun employee)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2)
  | f1 > f2   = g1
  | otherwise = g2

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------



treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold = undefined
--treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)
--treeFold e f node = f (Node { rootLabel = rootLabel node, subForest = treeFold e f })



combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs = undefined


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel = undefined


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

--maxFun :: Tree Employee -> GuestList
--maxFun = undefined


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = undefined
