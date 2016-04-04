
----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log

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

-- Convert String to Int
getInt :: String -> Int
getInt x = read x::Int

-- Determine error type
getErrorType :: String -> MessageType
getErrorType x = Error $ getInt x

-- List of strings to LogMessage
getLogMessage :: [String] -> LogMessage
getLogMessage var = case var of ("E":y:z:xs) -> LogMessage (getErrorType y) (getInt z) (unwords xs)
                                ("I":y:xs) -> LogMessage Info (getInt y) (unwords xs)
                                ("W":y:xs) -> LogMessage Warning (getInt y) (unwords xs)
                                _ -> Unknown "This is not in the right format"

-- Parse message
parseMessage :: String -> LogMessage
parseMessage = getLogMessage . words

-- Paser document
parse :: String -> [LogMessage]
parse x = map parseMessage elements
    where elements = lines x

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>>
--

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf = Node Leaf lm Leaf
insert lmInsert@(LogMessage _ timeStampInsert _) mt@(Node left lm@(LogMessage _ timeStamp _) right)
  | timeStampInsert == timeStamp = mt
  | timeStampInsert < timeStamp  = Node (insert lmInsert left) lm right
  | timeStampInsert > timeStamp  = Node left lm (insert lmInsert right)
insert lm@LogMessage{} (Node _ (Unknown _) _) = Node Leaf lm Leaf
insert lm@LogMessage{} (Node _ LogMessage{} _) = Node Leaf lm Leaf

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>>
--

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = Node Leaf x Leaf
build (x:xs) = insert x (build xs)

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

-- Check if log line is error
isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

-- Check if error is relevant
isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error n) _ _) = n >= 50
isRelevant _ = False

-- Get messages according to relevance
getMessages :: [LogMessage] -> [String]
getMessages [] = []
getMessages [Unknown _] = []
getMessages (Unknown _: (_ : _)) = []
getMessages (lm@(LogMessage _ _ message) : xs)
  | isError lm && isRelevant lm = message : getMessages xs
  | otherwise                   = getMessages xs


-- Messages from ordered list
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMessages . inOrder . build

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

whoDidIt :: String
whoDidIt = undefined
