{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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

