import Data.List (sortBy,groupBy)

lsort :: [[a]] -> [[a]]
lsort = sortBy (\a b -> (length a) `compare` (length b))

lfsort :: [[a]] -> [[a]]
lfsort = map snd.(sortBy (\(n,_) (m,_) -> n `compare` m)).freq.lsort
            where freq :: [[a]] -> [(Int,[[a]])]
                  freq = map (\x -> (length x,x)).(groupBy (\a b -> (length a) == (length b)))