split :: [a] -> Int -> ([a],[a])
split xs 0 = ([], xs)
split (x:xs) n = (x:f, s)
                 where (f,s) = split xs $ n-1
