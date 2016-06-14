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

glCons :: Employee -> GuestList -> GuestList
glCons e@Emp{empFun=f} (GL l tf) = GL (e:l) (tf+f)


moreFun :: GuestList -> GuestList -> GuestList
moreFun l1@(GL _ f1) l2@(GL _ f2)
  | f1>f2     = l1
  | otherwise = l2

instance Monoid GuestList where
  mempty = GL [] 0
  GL l1 f1 `mappend` GL l2 f2 = GL (l1++l2) (f1+f2)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

treeFold :: (a -> b -> b) -> b -> Tree a -> [b]
treeFold f z (Node x []) = [f x z]
treeFold f z (Node x ns) = [f x y | n <- ns, y <- treeFold f z n]


--treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
--treeFold e _ Empty        = e
--treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

--treeSum :: Tree Integer -> Integer
--treeSum Empty     = 0
--treeSum (Node l x r)  = x + treeSum l + treeSum r


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

compareTwoList :: (GuestList, GuestList) -> GuestList
compareTwoList (a, b) = moreFun a b

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel _ []     = (mempty, mempty)
nextLevel e (x:xs) = (final , (glCons e final))
   where
     best  = compareTwoList x
     other = nextLevel e xs
     final = best `mappend` (compareTwoList other)

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun (Node e@(Emp { empFun=f}) [])  = GL [e] f
maxFun (Node e x)                     = compareTwoList ( nextLevel e ( foo x ( (Node e []) :x) ) )

foo :: [Tree Employee] -> [Tree Employee] -> [(GuestList, GuestList)]
foo [] (y:_)      = [(mempty, maxFun y)]
foo (x:xs) (y:ys) = (maxFun x, maxFun y) : (foo xs ys)
foo _ _           = [(mempty, mempty)]

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = do
  company <- readEmployees "company.txt"
  print company

readEmployees :: FilePath -> IO GuestList
readEmployees path = do
  alldata <- readFile path
  return (stringToTree alldata)

stringToTree :: String -> GuestList
stringToTree a = mempty
