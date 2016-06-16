{-# LANGUAGE InstanceSigs #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 10
--
----------------------------------------------------------------------

module AParser where

-- base
import Control.Applicative
import Data.Char


newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }


-- |
--
-- >>> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- >>> runParser (satisfy isUpper) "abc"
-- Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []          = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing


-- |
--
-- >>> runParser (char 'x') "xyz"
-- Just ('x',"yz")

char :: Char -> Parser Char
char c = satisfy (== c)


posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = Parser pb where
      pb x = case runParser pa x of
                  Nothing    -> Nothing
                  Just (a,b) -> Just (first f (a,b))

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser p where
    p x = Just (a, x)


  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser pb where
          pb x = case runParser pf x of
              Nothing -> Nothing
              Just (a, b) -> runParser (fmap a pa) b

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing

abParser :: Parser (Char, Char)
abParser = foo1 <$> char 'a' <*> char 'b'

foo1 :: Char -> Char -> (Char, Char)
foo1 x y= (x,y)


-- |
--
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing

abParser_ :: Parser ()
abParser_ = foo2 <$> char 'a' <*> char 'b'

foo2 :: Char -> Char -> ()
foo2 _ _= ()

-- |
--
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")


intPair :: Parser [Integer]
intPair = foo3 <$> posInt <*> char ' ' <*> posInt

foo3 :: Integer -> Char -> Integer -> [Integer]
foo3 x _ y = [x,y]

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

instance Alternative Parser where
  empty :: Parser a
  empty = Parser f where
    f _ = Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser c where
    c x = case r of
            Nothing -> runParser p2 x
            otherwise -> r
            where
              r= runParser p1 x


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing

intOrUppercase :: Parser ()
intOrUppercase = (returnsEmpty <$> posInt) <|> (returnsEmpty <$> satisfy isUpper)

returnsEmpty :: a -> ()
returnsEmpty _ = ()
