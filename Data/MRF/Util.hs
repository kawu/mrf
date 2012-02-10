module Data.MRF.Util
( partition
) where

import Data.List (transpose)

partition :: Int -> [a] -> [[a]]
partition k = transpose . group k
    where group k [] = []
          group k xs = take k xs
                     : (group k $ drop k xs)
