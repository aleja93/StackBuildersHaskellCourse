
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
-- "I 3466 pci_hcd beed VRAM=2)
-- "I 2788 from?'"
-- "I 1108 had vanished completely."
-- "W 159 [drm/ing locouchabet 1000000 pags: bus not D32 us=io2/ing 133"

m1=parseMessage "I 15 Message1"
m2=parseMessage "I 20 Message2"
m3=parseMessage "I 30 Message3"
m4=parseMessage "I 40 Message4"
m5=parseMessage "I 50 Message5"
u=parseMessage "This is not the formatt"
t1=Leaf
t2=Node Leaf m2 Leaf
t3=Node t2 m3 Leaf


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
  | ts2>ts1 = Node (insert n mti) p mtr
  | otherwise = Node mti p (insert n mtr)
insert n@(LogMessage _ ts1 _) (Node Leaf p@(LogMessage _ ts2 _) Leaf)
  | ts2>ts1 = Node (Node Leaf n Leaf) p Leaf
  | otherwise = Node Leaf p (Node Leaf n Leaf)

listLogMessage=parse "I 15 Message1\nI 20 Message2\nI 30 Message3\nI 40 Message4\n 50 Message5"
listLogMessage0=parse ""
listLogMessage1=parse "I 15 Message1"
listLogMessage2=parse "I 30 Message30\nI 20 Message2\nI 50 Message3\nI 40 Message4\nW 30 Message5"

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x (build xs)


treeM= build listLogMessage2

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf= []
inOrder (Node Leaf m Leaf) = [m]
inOrder (Node mtl@(Node _ _ _) m Leaf) = inOrder(mtl)++[m]
inOrder (Node Leaf m mtr@(Node _ _ _)) = [m]++inOrder(mtr)
inOrder (Node mtl@(Node _ _ _) m mtr@(Node _ _ _)) = inOrder(mtl)++[m]++inOrder(mtr)

messages= "I 20 Which brought \nW 60 it is \nI 55 Today \nThis is not\nI 11 had vanished completely.\nW 61 a great day"
listM=parse messages
listMinOrder= inOrder (build listM)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []=[]
whatWentWrong ( (LogMessage _ ts m) :xs)
  | ts>50 = [m]++ whatWentWrong( inOrder (build xs))
  | otherwise = whatWentWrong(inOrder (build xs))
