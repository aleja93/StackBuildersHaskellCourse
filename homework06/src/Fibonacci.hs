----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 06
--
----------------------------------------------------------------------

module Fibonacci where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fib :: Integer -> Integer
fib x
  | x<0       = -1
  | x==0      =  0
  | x==1      =  1
  | otherwise =  fib (x-1) + fib (x-2)


fibs1 :: [Integer]
fibs1 = map fib [0..]


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = 0: fun 0 1

fun :: Integer -> Integer -> [Integer]
fun a b = b:fun b (a+b)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show (Cons a x)= take 100 (show a ++" "++show x)

streamToList :: Stream a -> [a]
streamToList (Cons a x) = a:streamToList x

listToStream :: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = listToStream(repeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a x) = Cons (f a) (streamMap f x)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a ( streamFromSeed f (f a))

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = streamMap maxNumberOfTwo nats

maxNumberOfTwo :: Integer -> Integer
maxNumberOfTwo x = if even x then 1+ maxNumberOfTwo (x `div` 2) else 0

takeOneCero :: [Integer] -> [Integer]
takeOneCero (x:xs) = if x/=0 && x/=1 then x:takeOneCero xs else takeOneCero xs
----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

x :: Stream Integer
x = undefined


----------------------------------------------------------------------
-- Exercise 7 (Optional)
----------------------------------------------------------------------

fib4 :: Integer -> Integer
fib4 = undefined
