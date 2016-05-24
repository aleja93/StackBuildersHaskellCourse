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
-- ["ABCD", "BD", "C", "D"]
-- >>> skips "hello!"
-- ["hello!", "el!", "l!", "l", "o", "!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True, False]
-- [[True,False], [False]]
-- >>> skips []
-- []

takeNthElements :: [a] -> Int -> [a]
takeNthElements [] _ = []
takeNthElements x n
  | n<=(length x) = [(x !! (n-1))] ++(takeNthElements(drop n x) n)
  | otherwise =[]


takeNTimes :: [a] -> Int -> [[a]]
takeNTimes _ 0     = []
takeNTimes x n = [(takeNthElements x n)] ++ takeNTimes x (n - 1)


skips :: [a] -> [[a]]
skips x = reverse (takeNTimes x (length x))

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
-- [1]

localMaximaOneNumber :: Integer -> Integer -> Integer -> Bool
localMaximaOneNumber b n a = (n>b) && (n>a)

localMaxima :: [Integer] -> [Integer]
localMaxima []= []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs)
  | (localMaximaOneNumber x y z) = [y] ++ localMaxima (y:z:xs)
  | otherwise  = localMaxima (y:z:xs)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"


--
-- countElement :: [a] -> a -> Integer
-- countElement x b = foldl(+) 0 (filter (==b) x)

countElement :: [a] -> [a] -> [Integer]
countElement _ [] = []
countElement l (x:xs) = [foldl(+) 0 (filter (==x) l)]++ (countElement l xs)


histogram :: [Integer] -> String
histogram = undefined
