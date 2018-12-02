module Utils.Array where

import Data.Array as Array
import Prelude ((+), (==))

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


isEmpty :: forall a. Array a -> Boolean
isEmpty array =
    if Array.length array == 0 then
        true
    else
        false
