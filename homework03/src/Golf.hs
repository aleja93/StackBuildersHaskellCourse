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

takeNthElements :: [a] -> Int -> [a]
takeNthElements [] _ = []
takeNthElements x n
  | n<=length x = (x !! (n-1)): takeNthElements(drop n x) n
  | otherwise =[]


takeNTimes :: [a] -> Int -> [[a]]
takeNTimes _ 0     = []
takeNTimes x n = takeNthElements x n : takeNTimes x (n - 1)


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
-- []

localMaximaOneNumber :: Integer -> Integer -> Integer -> Bool
localMaximaOneNumber b n a = (n>b) && (n>a)

localMaxima :: [Integer] -> [Integer]
localMaxima []= []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs)
  | localMaximaOneNumber x y z = y : localMaxima (y:z:xs)
  | otherwise  = localMaxima (y:z:xs)


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"

countElement :: [Integer] -> [Integer] -> [Integer]
countElement _ [] = []
countElement l (x:xs) = toInteger(length (filter (==x) l)) : countElement l xs

takeOne :: [Integer] -> [Integer]
takeOne []=[]
takeOne (x:xs)
  | x>0 = (x-1) : takeOne xs
  | otherwise = x : takeOne xs

-- c= countElement x [0,1,2,3,4,5,6,7,8,9]

fAst ::Integer -> Char
fAst n
  | n>0 = '*'
  | otherwise = ' '

asterisk :: [Integer] -> String
asterisk []=[]
asterisk x
  | sum x > 0 =  map fAst x ++"\n"++ asterisk(takeOne x)
  | otherwise =""

asteriskR :: [Integer] -> String
asteriskR a= drop 1 (reverse (asterisk a)) ++ "\n==========\n0123456789\n"

histogram :: [Integer] -> String
histogram x = asteriskR ( countElement x [9,8,7,6,5,4,3,2,1,0])
