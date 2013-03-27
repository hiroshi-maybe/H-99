insertAt :: a -> [a] -> Int -> [a]
insertAt v xs n = take (n-1) xs ++ v:drop (n-1) xs