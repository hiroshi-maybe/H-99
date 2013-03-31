primesR :: Int -> Int -> [Int]
primesR l u = filter (\x -> not $ any (\y -> x `mod` y == 0) [2..sqrtNum x]) [l..u]
              where sqrtNum = truncate.sqrt.fromIntegral