dropEvery :: [a] -> Int -> [a]
dropEvery xs n | length xs < n = xs
               | otherwise = take (n-1) xs ++ dropEvery (drop n xs) n