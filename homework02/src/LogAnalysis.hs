{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log
import Data.Char

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

getFirstNumber :: String -> String
getFirstNumber [] = []
getFirstNumber (x:xs)
  | isDigit x = x:[] ++ getFirstNumber(xs)
  | otherwise = []

getRestMessage :: String -> String -> String
getRestMessage number s = drop (length number) s


parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage (l:s) = case l of 'E' ->  LogMessage (Error (read sn)) (read ts) (tail (getRestMessage ts (tail s2)))
                               'I' ->  LogMessage Info (read sn) (tail s2)
                               'W' ->  LogMessage Warning (read sn) (tail s2)
                               _ -> Unknown (l:s)
                               where
                               sn= getFirstNumber (tail s)
                               s2= getRestMessage sn (tail s)
                               ts= getFirstNumber (tail s2)

getLineString :: String -> String
getLineString []=[]
getLineString (x:xs)
  | (ord x) /= 10 = [x]++ getLineString(xs)
  | otherwise = []

parse :: String -> [LogMessage]
parse [] = []
parse x = [parseMessage(l)]++parse(rest)
          where
          l=getLineString x
          rest= getRestMessage ( l++" ") x

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>>
--

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) mto = mto
insert m Leaf = Node Leaf m Leaf
insert n@(LogMessage _ ts1 _) (Node mti@(Node _ _ _) p@(LogMessage _ ts2 _) Leaf)
  | ts2>ts1 = Node (insert n mti) p Leaf
  | otherwise = Node mti p (Node Leaf n Leaf)
insert n@(LogMessage _ ts1 _) (Node Leaf p@(LogMessage _ ts2 _) mtr@(Node _ _ _))
  | ts2>ts1 = Node (Node Leaf n Leaf) p mtr
  | otherwise = Node Leaf p (insert n mtr)
insert n@(LogMessage _ ts1 _) (Node mti@(Node _ _ _) p@(LogMessage _ ts2 _) mtr@(Node _ _ _))
  | ts2>ts1 = Node (insert n mti) p Leaf
  | otherwise = Node Leaf p (insert n mtr)
insert n@(LogMessage _ ts1 _) (Node Leaf p@(LogMessage _ ts2 _) Leaf)
  | ts2>ts1 = Node (Node Leaf n Leaf) p Leaf
  | otherwise = Node Leaf p (Node Leaf n Leaf)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>>
--

build :: [LogMessage] -> MessageTree
build = undefined

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

whoDidIt :: String
whoDidIt = undefined
