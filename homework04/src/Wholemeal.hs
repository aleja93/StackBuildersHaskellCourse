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
fun3 n = if even n then (n `div` 2) else (3*n + 1)

fun2' :: Integer -> Integer
fun2' n = sum (filter even (takeWhile (>1) (iterate (\x -> if even x then (x `div` 2) else (3*x + 1)) n)))


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
foldTree = undefined

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
xor = undefined

-- |
--
-- >>> map' (+1) [1,2,3]
-- [2,3,4]

map' :: (a -> b) -> [a] -> [b]
map' = undefined

-- Optional

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = undefined

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram = undefined
