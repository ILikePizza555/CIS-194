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
insert insertMessage@(LogMessage _ insertTimestamp _) (Node Leaf currentMessage@(LogMessage _ currentTimestamp _) Leaf)
    | insertTimestamp < currentTimestamp = Node newTree currentMessage Leaf
    | insertTimestamp > currentTimestamp = Node Leaf currentMessage newTree
    where newTree = (Node Leaf insertMessage Leaf)
insert insertMessage@(LogMessage _ insertTimestamp _) (Node Leaf currentMessage@(LogMessage _ currentTimestamp _) rightTree)
    | insertTimestamp < currentTimestamp = Node newTree currentMessage rightTree
    | insertTimestamp > currentTimestamp = insert insertMessage rightTree
    where newTree = (Node Leaf insertMessage Leaf)
insert insertMessage@(LogMessage _ insertTimestamp _) (Node leftTree currentMessage@(LogMessage _ currentTimestamp _) Leaf)
    | insertTimestamp < currentTimestamp = insert insertMessage leftTree
    | insertTimestamp > currentTimestamp = Node leftTree currentMessage newTree
    where newTree = (Node Leaf insertMessage Leaf)
insert insertMessage@(LogMessage _ insertTimestamp _) (Node leftTree (LogMessage _ currentTimestamp _) rightTree)
    | insertTimestamp < currentTimestamp = insert insertMessage leftTree
    | insertTimestamp > currentTimestamp = insert insertMessage rightTree