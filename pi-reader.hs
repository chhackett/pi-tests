-- Read digits of pi
-- Get all sequences of digits of length 2..6
-- For each sequence, add them to the trie
--      key = index of first digit
--      value = sequence of digits

{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List
import Control.Monad
import TrieNode as TN

main = do
    handle <- openFile "pi_dec_1m.txt" ReadMode

    -- Skip "3." at the beginning of the file
    firstChar <- hGetChar handle
    firstChar <- hGetChar handle

    pi_1million <- TIO.hGetContents handle

    print "Building pi trie..."
    let pieceofpi = T.take 400000 pi_1million
        piTrie = buildTrie pieceofpi 0 TN.empty

    forever $ do
        putStr "Enter a sequence of up to 6 digits:"
        seq <- getLine
        print . reverse $ getRing seq piTrie

    hClose handle

buildTrie :: T.Text -> Integer -> TrieNode String -> TrieNode String
buildTrie pi i trie =
  if T.null pi
  then trie
  else buildTrie (T.tail pi) (i+1) (addNodes pi i trie)
  where
    addNodes :: T.Text -> Integer -> TrieNode String -> TrieNode String
    addNodes t = insertAll [ T.take size t | size <- [1..6] ]

    insertAll :: [T.Text] -> Integer -> TrieNode String -> TrieNode String
    insertAll ts i trie' = foldl textToNode trie' ts

    textToNode trie' t = TN.insert (T.unpack t) (show i) trie'

getRing :: String -> TrieNode String -> [String]
getRing n trie = getRing' n [n]
    where getRing' n' result = case TN.get n' trie of
            Nothing  -> result
            Just pos -> if pos `elem` result
                    then pos:result
                    else getRing' pos (pos:result)
