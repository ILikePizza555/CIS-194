{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--http://www.cis.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

--Parses a single line of the log file
parseLine :: String -> LogMessage
parseLine logString =
    case logStringWords of  "I":timestamp:message -> LogMessage Info (read timestamp) (unwords message)
                            "W":timestamp:message -> LogMessage Warning (read timestamp) (unwords message)
                            "E":severity:timestamp:message -> LogMessage (Error (read severity)) (read timestamp) (unwords message)
                            _ -> Unknown logString
    where logStringWords = words logString

--Parses a whole file. Definined in excersize 1
parse :: String -> [LogMessage]
parse file = [parseLine l | l <- lines file]

--Inserts a LogMessage into a BST based on timestamp
insert :: LogMessage -> MessageTree -> MessageTree
insert insertMessage Leaf = Node Leaf insertMessage Leaf
insert insertMessage@(LogMessage _ insertTimestamp _) (Node leftTree currentMessage@(LogMessage _ currentTimestamp _) rightTree)
    | insertTimestamp < currentTimestamp = Node (insert insertMessage leftTree) currentMessage rightTree
    | insertTimestamp > currentTimestamp = Node leftTree currentMessage (insert insertMessage rightTree)

--Builds a BST of MessageTree from a list of logMessages
build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

--Does an inOrder traversal of the BST
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf message Leaf) = [message]
inOrder (Node ltree message rtree) = inOrder ltree ++ message : inOrder rtree

--Searches for every error with a severity of 50 or greater
search :: MessageTree -> Int -> [LogMessage]
search Leaf _ = []
search (Node Leaf message@(LogMessage (Error severity) _ _) Leaf) n
    | severity >= n = [message]
    | otherwise     = []
search (Node ltree message@(LogMessage (Error severity) _ _) rtree) n
    | severity >= n = search ltree n ++ message : search rtree n
    | severity <  n = search rtree n
search (Node ltree _ rtree) n = search ltree n ++ search rtree n

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessage = map (\(LogMessage _ _ message) -> message) (search (build logMessage) 50)