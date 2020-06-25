-- Read digits of pi
-- Get all sequences of digits of length 2..6
-- For each sequence, add them to the trie
--      key = index of first digit
--      value = sequence of digits

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import System.IO
import Data.List
import Control.Monad
import TrieNode as TN

main = do
    handle <- openFile "pi_dec_1m.txt" ReadMode

    -- Skip "3." at the beginning of the file
    firstChar <- hGetChar handle
    firstChar <- hGetChar handle

    pi_1million <- hGetContents handle

    print "Building pi trie..."
    let !pieceofpi = take 800000 pi_1million
        !piTrie = buildTrie pieceofpi

    forever $ do
        putStr "Enter a sequence of up to 6 digits:"
        seq <- getLine
        --print . reverse $ getRing seq piTrie
        print $ getIndex seq piTrie

    hClose handle

buildTrie :: String -> TrieNode String
buildTrie pi = let result = foldl' build (TN.empty, pi, 0) pi in first result
  where
    build :: (TrieNode String, String, Int) -> Char -> (TrieNode String, String, Int)
    build (!trie, !leftOverPi, i) _ = (addNodes leftOverPi i trie, tail leftOverPi, i+1)

    addNodes :: String -> Int -> TrieNode String -> TrieNode String
    addNodes leftOverPi i !trie = insertAll [ take size leftOverPi | size <- [1..6] ] i trie

    insertAll :: [String] -> Int -> TrieNode String -> TrieNode String
    insertAll piecesOfPi i !trie' = foldl textToNode trie' piecesOfPi
        where
            textToNode !trie' pieceOfPi = TN.insert pieceOfPi (show i) trie'

    first :: (a, b, c) -> a
    first (a, _, _) = a

getIndex :: String -> TrieNode String -> String
getIndex n trie =
  case TN.get n trie of
        Nothing  -> "Not found"
        Just pos -> pos

getRing :: String -> TrieNode String -> [String]
getRing n trie = getRing' n [n]
    where getRing' n' result = case TN.get n' trie of
            Nothing  -> result
            Just pos -> if pos `elem` result
                    then pos:result
                    else getRing' pos (pos:result)
