myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
-- myReverse xs = last xs : (myReverse $ init xs)
