-- Copy of solution 26.
combinations :: (Eq a) =>Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n (x:xs) = x_inc ++ x_exc
                        where x_inc = do cand <- combinations (n-1) xs
                                         return (x:cand)
                              x_exc = if n>length xs then [] else combinations n xs

-- Adding REST elements to return value of predefined "combinations"
combination_pair :: (Eq a) => Int -> [a] -> [([a],[a])]
combination_pair n xs = do comb <- combinations n xs
                           let rest = filter (\x -> x `notElem` comb) xs
                           return (comb,rest)

group' :: (Eq a) => [Int] -> [a] -> [[[a]]]
group' [] ys | null ys = [[]]
             | otherwise = error "invalid list"
group' (x:xs) ys = do (comb,rest) <- combination_pair x ys
                      other_comb <- group' xs rest
                      return (comb:other_comb)
