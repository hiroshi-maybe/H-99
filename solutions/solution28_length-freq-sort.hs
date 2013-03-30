import Data.List (sortBy,groupBy)

lsort :: [[a]] -> [[a]]
lsort = sortBy (\a b -> (length a) `compare` (length b))

lfsort :: [[a]] -> [[a]]
lfsort = concat.lsort.freq.lsort
         where freq :: [[a]] -> [[[a]]]
               freq = groupBy (\a b -> (length a) == (length b))