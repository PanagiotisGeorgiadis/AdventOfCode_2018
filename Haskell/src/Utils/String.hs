module Utils.String where

import Prelude

split :: Char -> [Char] -> [[Char]]
split criterion str          = go [] "" str
    where go res temp (x:xs)
            | x == criterion = go (res ++ [temp]) "" xs
            | otherwise      = go res (temp ++ [x]) xs
          go res temp []     = res ++ [temp]


explode :: [Char] -> [[Char]]
explode = go []
    where go res (x:xs) = go (res ++ [[x]]) xs
          go res []     = res
