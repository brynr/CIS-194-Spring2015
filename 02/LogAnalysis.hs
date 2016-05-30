{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Prelude

readsMaybeInt :: String -> Maybe Int 
readsMaybeInt x = case (reads x :: [(Int, String)]) of
               [] -> Nothing
               (n,""):_ -> Just n
               (_, _):_ -> Nothing

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType [] = Nothing
parseMessageType ("I":xs) = Just (Info, xs)
parseMessageType ("W":xs) = Just (Warning, xs)
parseMessageType ("E":numStr:xs) = case readsMaybeInt numStr of 
                                    Nothing -> Nothing
                                    Just n -> Just (Error n, xs)
parseMessageType _ = Nothing

parseTimeStamp :: [String] -> Maybe (TimeStamp, [String])
parseTimeStamp [] = Nothing
parseTimeStamp (tStr:xs) = case readsMaybeInt tStr of
                                Nothing -> Nothing
                                Just t -> Just (t, xs)

parseTypeAndTime :: [String] -> Maybe (MessageType, TimeStamp, [String])
parseTypeAndTime [] = Nothing
parseTypeAndTime xs = case parseMessageType xs of
                        Nothing -> Nothing
                        Just (msgType, xs') -> case parseTimeStamp xs' of
                                                 Nothing -> Nothing
                                                 Just (time, xs'') -> Just (msgType, time, xs'')

parseMessageFromList :: [String] -> LogMessage
parseMessageFromList [] = Unknown ""
parseMessageFromList xs = case parseTypeAndTime xs of
                            Nothing -> Unknown $ unwords xs
                            Just (msgType, time, remaining) -> LogMessage msgType time (unwords remaining)



parseMessage :: String -> LogMessage
parseMessage x = parseMessageFromList (words x)

parseLogfile :: String -> [LogMessage]
parseLogfile fileStr = map parseMessage $ lines fileStr

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) node@(Node left nodeMsg@(LogMessage _ nodeTime _) right)
  | time < nodeTime = Node (insert msg left) nodeMsg right
  | time > nodeTime = Node left nodeMsg (insert msg right)
  | time == nodeTime = node -- This shouldn't happen.

buildInto :: [LogMessage] -> MessageTree -> MessageTree
buildInto [] tree = tree
buildInto (x:xs) tree = buildInto xs $ insert x tree

build :: [LogMessage] -> MessageTree
build xs = buildInto xs Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ msg : inOrder right

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error errLevel) _ _) = errLevel > 50
isRelevant _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map (\(LogMessage _ _ str) -> str) $ inOrder . build $ filter isRelevant xs
