{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 05
--
----------------------------------------------------------------------

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
-- (2+3)*4
-- >>> eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20
-- True

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add a b)= eval a + eval b
eval (ExprT.Mul a b)= eval a * eval b

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-------------------
-- *
-- Calc> parseExp ExprT.Lit ExprT.Add ExprT.Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- Calc> parseExp ExprT.Lit ExprT.Add ExprT.Mul "2+3*4"
-- Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
-- Calc> parseExp ExprT.Lit ExprT.Add ExprT.Mul "2+3*"
-- Nothing
------

evalStr :: String -> Maybe Integer
evalStr a = case expression of
                   Nothing -> Nothing
                   Just x  -> Just (eval x)
                   where
                     expression = parseExp ExprT.Lit ExprT.Add ExprT.Mul a


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- class Listable a where
--   toList :: a -> [Int]
--   toList :: Listable a => a -> [Int]
--
-- instance Listable Int where
  -- toList :: Int -> [Int]
--   toList x = [x]

-- instance Listable Bool where
--   toList True  = [1]
--   toList False = [0]

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = ExprT.Lit x
  add a b = ExprT.Add a b
  mul a b = ExprT.Mul a b


reify :: ExprT -> ExprT
reify = id

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

----------------------------------------------------------------------
-- Exercise 5 (do this OR exercise 6)
----------------------------------------------------------------------

compile :: String -> Maybe Program
compile = undefined


----------------------------------------------------------------------
-- Exercise 6 (do this OR exercise 5)
----------------------------------------------------------------------

-- |
--
-- >>> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
