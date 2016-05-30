{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import qualified Data.ByteString.Lazy as ByteL
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)
import Control.Applicative
import Data.ByteString.Char8 ()

import qualified Data.ByteString as ByteS
import qualified Data.Map.Strict as Map
import Data.Word (Word8)

import Parser

-- Exercise 1 -----------------------------------------

pairwiseXorFiles :: FilePath -> FilePath -> IO [Word8]
pairwiseXorFiles f1 f2 = ByteS.zipWith xor <$> ByteS.readFile f1 <*> ByteS.readFile f2

getSecret :: FilePath -> FilePath -> IO ByteS.ByteString
getSecret f1 f2 = do
    bytes <- pairwiseXorFiles f1 f2
    return $ ByteS.pack $ filter (/=0) bytes

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteL.ByteString -> FilePath -> IO ()
decryptWithKey key file = do
    encFile <- ByteS.readFile (file ++ ".enc")
    ByteL.writeFile file $ ByteL.pack $ ByteL.zipWith xor (ByteL.cycle key) (ByteL.fromStrict encFile)
    return ()

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = undefined

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs = undefined

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey (ByteL.fromStrict key) vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

