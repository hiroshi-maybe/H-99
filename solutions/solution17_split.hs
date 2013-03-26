split :: [a] -> Int -> [[a]]
split xs 0 = [[], xs]
split (x:xs) n = (x: head' rest) : tail' rest
               where rest = split xs $ n-1

-- Do not use any predefined predicates
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs
