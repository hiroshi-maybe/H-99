range :: Int -> Int -> [Int]
range n m = take (m-n+1).drop (n-1) $ [1..]
-- range x y = [x .. y]
-- range x y = enumFromTo x y