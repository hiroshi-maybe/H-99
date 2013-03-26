slice :: [a] -> Int -> Int -> [a]
slice _ _ 0 = []
slice (x:xs) n m | n>m = []
                 | n<=1 && m>0 = x : rest
                 | otherwise = rest
                 where rest = slice xs (n-1) (m-1)