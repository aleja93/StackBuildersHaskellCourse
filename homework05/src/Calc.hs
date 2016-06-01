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
import VarExprT
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
eval (ExprT.Lit x)  = x
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


class Expr a where
  lit :: Integer -> a
  add :: a -> a  -> a
  mul :: a -> a  -> a

instance Expr ExprT where
  lit x   = ExprT.Lit x
  add a b = ExprT.Add a b
  mul a b = ExprT.Mul a b


reify :: ExprT -> ExprT
reify = id

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

instance Expr Integer where
  lit x   = x
  add a b = a+b
  mul a b = a*b

instance Expr Bool where
  lit x   = if  x>0 then True else False
  add a b = a || b
  mul a b = a && b


newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x                     = MinMax x
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit x                 = Mod7 x
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a+b) 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod (a*b) 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer     -- Just (-7)
testBool     = testExp :: Maybe Bool        -- Just True
testMM       = testExp :: Maybe MinMax      -- Just (MinMax 5)
testSat      = testExp :: Maybe Mod7        -- Just (Mod7 0)


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

class HasVars a where
  var :: String -> a

instance Expr VarExprT where
  lit x   = VarExprT.Lit x
  add a b = VarExprT.Add a b
  mul a b = VarExprT.Mul a b

instance HasVars VarExprT where
  var x   = VarExprT.Var x



withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
