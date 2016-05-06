----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
--
----------------------------------------------------------------------

module Party where

import Employee
import Data.Tree
import Data.List

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL em fun) = GL (em ++ [e]) (fun + empFun e)

instance Monoid GuestList where
  mempty                       = GL [] 0
  mappend(GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2 = if g1 > g2 then g1 else g2


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f val tree = f (rootLabel tree) mtree
  where mtree = map (treeFold f val) (subForest tree)


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gl = (withBoss, noBoss)
  where withBoss = GL [e] (empFun e)
        noBoss   = mconcat $ map (uncurry moreFun) gl


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . (treeFold nextLevel mempty)


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = do
  employees <- readFile "company.txt"
  putStrLn . formatGL . maxFun . read $ employees

formatGL :: GuestList -> String
formatGL (GL em fun) =
  "Total fun: " ++ show fun ++ "\n" ++ list
  where list = unlines. sort . map empName $ em


-- glCons (Emp { empName = "Rodrigo", empFun = 10}) (GL [Emp { empName = "A", empFun = 1},Emp { empName = "B", empFun = 2}] 3)
-- moreFun (Emp { empName = "A", empFun = 10}) (Emp { empName = "X", empFun = 20})
-- let y = glCons (Emp { empName = "Y", empFun = 20}) (GL [Emp { empName = "A", empFun = 1},Emp { empName = "B", empFun = 2}] 3)
-- let e = (Emp { empName = "E", empFun = 1})
-- let x = glCons (Emp { empName = "X", empFun = 10}) (GL [Emp { empName = "A", empFun = 1},Emp { empName = "B", empFun = 2}] 3)
