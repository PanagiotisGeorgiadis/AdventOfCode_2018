module Utils.Array where

import Prelude ((+), (==))
import Data.Array as Array

sum :: Array Int -> Int
sum =
    Array.foldl ((+)) 0


count :: Int -> Array Int -> Int
count needle haystack =
    Array.foldl
        (\res item ->
            if item == needle then
                res + 1
            else
                res
        )
        0
        haystack
