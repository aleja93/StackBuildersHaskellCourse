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

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x
  | x<0       = []
  | otherwise = (x `mod` 10): [] ++ toDigitsRev (x `div` 10)


toDigits:: Integer -> [Integer]
toDigits x= reverse (toDigitsRev x)

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []=[]
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs)
  | length xs `mod` 2 == 1 = x:y*2: doubleEveryOther xs
  | otherwise = x*2:y: doubleEveryOther xs

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> sumDigits [16,7,12,5]
-- 22

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x>9 = sumDigits xs +(x `mod` 10) + (x `div` 10)
  | otherwise = sumDigits xs +x

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False

validate :: Integer -> Bool
validate x
  | sumDigits(doubleEveryOther(toDigits x)) `mod` 10 == 0 = True
  | otherwise =False

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg)

-- |
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a,b)]
hanoi n a b c =
   hanoi (n-1) a c b  ++
   hanoi 1 a b c  ++
   hanoi (n-1) c b a


----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' = undefined
