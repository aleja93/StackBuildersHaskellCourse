module Main where

import JoinList
import Editor
import Sized
import Scrabble

l1 = "Hello, This buffer is for notes you don't want to save, and for"
l2 = "evaluation of steam valve coefficients."
l3 = "To load a different file, type the character L followed"
l4 = "by the name of the file."


initialBuffer = Single (scoreString l1 , Size 1 ) l1 +++ Single (scoreString l2 , Size 1 ) l2 +++ Single (scoreString l3 , Size 1 ) l3 +++ Single (scoreString l4 , Size 1 ) l4

main = runEditor editor initialBuffer
