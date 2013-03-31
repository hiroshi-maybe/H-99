-- Copy of solution 39.
primesR :: Int -> Int -> [Int]
primesR l u = filter (\x -> not $ any (\y -> x `mod` y == 0) [2..sqrtNum x]) [l..u]
              where sqrtNum = truncate.sqrt.fromIntegral

goldbach :: Int -> (Int,Int)
goldbach n | n `mod` 2 /= 0 = error "Input number is not even."
           | otherwise = head $ filter (\(x,y) -> x+y == n) primePair
                         where primePair = do x <- primes
                                              y <- dropWhile (<=x) primes
                                              return (x,y)
                               primes = primesR 2 n
