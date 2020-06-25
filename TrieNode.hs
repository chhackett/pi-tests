{-# LANGUAGE BangPatterns #-}

module TrieNode where

import Data.Maybe

-- Trie definition - needs to support insert and get operations
data TrieNode a = TrieNode { value :: Maybe a,
                             children :: [(Char, TrieNode a)]
                           } deriving (Show, Read, Eq)

empty = TrieNode Nothing []
getKeys = map fst

insert :: Eq a => String -> a -> TrieNode a -> TrieNode a
insert [] _ !node  = node
insert [c] v !node =
    let !kids = children node; val = value node
    in if c `elem` getKeys kids
       then TrieNode val $! replaceNode c v kids
       else TrieNode val $! ((c, TrieNode (Just v) []):kids)
insert (c:cs) v !node =
    let !kids = children node; val = value node
    in if c `elem` getKeys kids
       then TrieNode val (map (\kid -> if fst kid == c
                                       then (c, insert cs v (snd kid))
                                       else kid) kids)
       else TrieNode val ((c, insert cs v empty):kids)

-- Given a key, value, and a list of (key, TrieNode) tuples, replace the
-- tuple with the given key with a new tuple (k, TrieNode value)
replaceNode :: Eq a => Char -> a -> [(Char, TrieNode a)] -> [(Char, TrieNode a)]
replaceNode c val =
    map (\node -> if fst node == c
                  then (c, TrieNode (put val $ snd node) (children $ snd node))
                  else node)
    where put val' !node' =
           if isNothing (value node')
           then Just val'
           else value node'

get :: Eq a => String -> TrieNode a -> Maybe a
get [] _     = Nothing
get [c] !t    = value =<< getValue c (children t)
get (c:cs) !t = case getValue c (children t) of
                     Nothing -> Nothing
                     Just t' -> get cs t'

getValue :: Char -> [(Char, TrieNode a)] -> Maybe (TrieNode a)
getValue c !ts = let ts' = filter (\t -> fst t == c) ts
                in if null ts' then Nothing else Just (snd $ head ts')
