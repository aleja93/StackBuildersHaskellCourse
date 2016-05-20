
import Data.Char

data MessageType = Info
  | Warning
  | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
  | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
  | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

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

-- "I 1578 Which brought them back again to the beginning of the conversation.\nW 3955 uhcd nosted)\nI 5071 #72"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) mto = mto
insert m Leaf = Node Leaf m Leaf
insert n@(LogMessage _ ts1 _) (Leaf p@(LogMessage _ ts2 _) Leaf)
  | ts2>ts1 = Node ((Node Leaf n Leaf) p Leaf)
  | otherwise = Node (Leaf p (Node Leaf n Leaf))
insert n@(LogMessage _ ts1 _) (Node mti@(Node _ _ _) (LogMessage _ ts2 _) mtr@(Node _ _ _))
  | ts2>ts1 = insert n mti
  | otherwise = insert n mtr
insert n@(LogMessage _ ts1 _) (Node Leaf p@(LogMessage _ ts2 _) mtr@(Node _ _ _))
  | ts2>ts1 = Node (Node Leaf n Leaf) p mtr
  | otherwise = insert n mtr
insert n@(LogMessage _ ts1 x) (Node mti@(Node _ _ _) p@(LogMessage _ ts2 _) Leaf)
  | ts2>ts1 = insert n mti
  | otherwise = Node mti p (Node Leaf n Leaf)
