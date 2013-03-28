combinations :: (Eq a) =>Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n (x:xs) = filter ((==n).length) (inc ++ combinations n xs)
                          where inc = do cand <- combinations (n-1) xs
                                         return (x:cand)
