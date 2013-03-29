combinations :: (Eq a) =>Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n (x:xs) = x_inc ++ x_exc
                        where x_inc = do cand <- combinations (n-1) xs
                                         return (x:cand)
                              x_exc = if n>length xs then [] else combinations n xs
