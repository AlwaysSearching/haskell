module LogAnalysis (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage line = case asList of
                    ("E":lvl:time:message) ->  LogMessage (Error $ read lvl) (read time) (unwords message) 
                    ("I":time:message) -> LogMessage Info (read time) (unwords message) 
                    ("W":time:message) -> LogMessage Warning (read time) (unwords message) 
                    _ -> Unknown line
                  where asList = words line 

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert log@(LogMessage _ _ _ ) Leaf = Node Leaf log Leaf
insert log_1@(LogMessage _ timestamp_1 _ ) (Node leftTree log_2@(LogMessage _ timestamp_2 _ ) rightTree)
    | timestamp_1 < timestamp_2 = Node (insert log_1 leftTree) log_2 rightTree
    | otherwise                 = Node leftTree log_2 (insert log_1 rightTree)
insert _ tree = tree


-- 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf 

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder (Node leftTree message rightTree) = inOrder leftTree ++ [message] ++ inOrder rightTree
inOrder _ = []


-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = messageList . inOrder . build . filter filterLogs 

-- filter out Logs that are not Error messages and Logs that are Not errors
filterLogs :: LogMessage -> Bool
filterLogs (LogMessage (Error lvl) _ _ ) 
    | lvl < 50  = False
    | otherwise = True
filterLogs _ = False

-- remove the messages from a list of logs
messageList :: [LogMessage] -> [String]
messageList ((LogMessage _ _ msg):logMessages) = msg : messageList logMessages 
messageList _ = [] 
