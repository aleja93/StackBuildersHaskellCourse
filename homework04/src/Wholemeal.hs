----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 04
--
----------------------------------------------------------------------

module Wholemeal where

import Data.Char

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' x= product (map (subtract 2) (filter even x))


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

fun :: Integer -> Integer
fun n
  | even n    = n `div` 2
  | otherwise = 3*n + 1

fun3 :: Integer -> Integer
fun3 n = if even n then n `div` 2 else 3*n + 1

fun2' :: Integer -> Integer
fun2' n = sum (filter even (takeWhile (>1) (iterate (\x -> if even x then x `div` 2 else 3*x + 1) n)))


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

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

data Tree a =
    Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

t1= insertNode 'A' Leaf
t2= insertNode 'B' t1
t3= insertNode 'C' t2
t4= insertNode 'D' t3
t5= insertNode 'E' t4
t6= insertNode 'F' t5
t7= insertNode 'G' t6
t8= insertNode 'H' t7
t9= insertNode 'I' t8

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node h Leaf a Leaf) = Node (h+1) Leaf a (Node 0 Leaf x Leaf)
insertNode x (Node h nodel@(Node _ _ _ _) a Leaf) = Node h nodel a (Node 0 Leaf x Leaf)
insertNode x (Node h Leaf a noder@(Node _ _ _ _)) = Node h (Node 0 Leaf x Leaf) a noder
insertNode x (Node h nodel@(Node _ _ _ _) a noder@(Node _ _ _ _))
  | checkNumberNodes noder==0 = Node (h+1) nodel a (insertNode x noder)
  | checkNumberNodes noder==1 = Node h nodel a (insertNode x noder)
  | checkNumberNodes nodel<2 = Node h (insertNode x nodel) a noder
  | checkNumberNodes nodel == checkNumberNodes noder = Node (h+1) nodel a (insertNode x noder)

checkNumberNodes ::Tree a -> Int
checkNumberNodes (Node _ Leaf _ Leaf) = 0
checkNumberNodes (Node _ (Node _ _ _ _) _ Leaf)=1
checkNumberNodes (Node _ Leaf _ (Node _ _ _ _))=1
checkNumberNodes (Node _ (Node _ _ _ _) _ (Node _ _ _ _))=2

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False

xor :: [Bool] -> Bool
xor xs = odd (foldl (\acc x -> if x then acc+1 else acc) 0 xs)

-- |
--
-- >>> map' (+1) [1,2,3]
-- [2,3,4]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Optional

--myFoldl :: (a -> b -> a) -> a -> [b] -> a
--myFoldl f base xs = foldr (\x acc -> (f x):acc) base xs

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> (x*2)+1) ( takeOutNotExistingNumbers list (takeOutRepetedNumbers(filter (<top) (map rule s))) )
  where
    top= fromIntegral (n+2)
    list = take (fromIntegral n) [1..]
    listp= take (fromIntegral top `div` 2) [1..]
    s = cartProd listp listp

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

rule :: (Integer,Integer) -> Integer
rule (a,b) = a+b+(2*a*b)

takeOutRepetedNumbers :: [Integer] -> [Integer]
takeOutRepetedNumbers []=[]
takeOutRepetedNumbers (x:xs)
  | length (filter (==x) (x:xs)) >1 = takeOutRepetedNumbers xs
  | otherwise = x:takeOutRepetedNumbers xs

takeOutNotExistingNumbers :: [Integer] -> [Integer] -> [Integer] -- takeOutNotExistingNumbers [1,2,3] [1] == [2,3]
takeOutNotExistingNumbers [] _ =[]
takeOutNotExistingNumbers (x:xs) y
  | x `elem` y = takeOutNotExistingNumbers xs y
  | otherwise = x:takeOutNotExistingNumbers xs y
