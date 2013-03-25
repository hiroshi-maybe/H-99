repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate n x) xs