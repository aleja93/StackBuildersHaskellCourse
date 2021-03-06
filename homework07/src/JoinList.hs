----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

--instance Monoid Size where
--  mempty  = Size 0
--  mappend = (+)

single :: Monoid m => a -> JoinList m a
single = Single mempty -- mempty shloug be 1 for this to work

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 `mappend` tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

n0= Single (Size 1) "ale" -- ALl SIngle have to be Size 1
n1= Single (Size 1) "rena"
n2= Single (Size 1) "nancy"
n3 = n0 +++ n1
n4 = n2 +++ n3

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

indexJ :: (Sized b, Monoid b)  => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ v)
  | i==0       = Just v
  | otherwise  = Nothing
indexJ i (Append _ l r)
  | m < (i+1)  = indexJ (i- m) r
  | otherwise  = indexJ i l
  where
    m  = getSize(size(tag l))

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty  = Empty
dropJ i t@(Single _ _)
  | i==0       = t
  | otherwise  = Empty
dropJ i (Append _ l r)
  | m >= i     = dropJ i l +++ r
  | otherwise  = dropJ (i- m) r
  where
    m  = getSize(size(tag l))

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty  = Empty
takeJ i t@(Single _ _)
  | i==0       = Empty
  | otherwise  = t
takeJ i (Append _ l r)
  | m >= i     = takeJ i l
  | otherwise  = l +++ takeJ (i- m) r
  where
    m  = getSize(size(tag l))


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------
-- scoreLine "yay " +++ scoreLine "haskell!" == Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

jlToString :: JoinList (Score, Size) String -> String
jlToString Empty          = []
jlToString (Single _ v)   = v
jlToString (Append _ l r) = jlToString l ++ jlToString r


jlFromString :: String -> JoinList (Score, Size) String
jlFromString [] = Empty
jlFromString a  = Single (scoreString a, Size 1) a

jlCount :: JoinList (Score, Size) String -> Int
jlCount Empty          = 0
jlCount (Single _ _)   = 1
jlCount (Append _ l r) = jlCount l + jlCount r


jlValue :: JoinList (Score, Size) String -> Int
jlValue Empty          = 0
jlValue (Single _ v)   = getScore(scoreString v)
jlValue (Append _ l r) = jlValue l + jlValue r

instance Buffer (JoinList (Score, Size) String) where
  toString          = jlToString
  fromString        = jlFromString
  line              = indexJ
  replaceLine n l b = takeJ n b +++ jlFromString l +++ dropJ (n+1) b
  numLines          = jlCount
  value             = jlValue
