-- Copy of solution 39.
primesR :: Int -> Int -> [Int]
primesR l u = filter (\x -> not $ any (\y -> x `mod` y == 0) [2..sqrtNum x]) [l..u]
              where sqrtNum = truncate.sqrt.fromIntegral

-- Copy of solution 40.
goldbach :: Int -> (Int,Int)
goldbach n | n `mod` 2 /= 0 = error "Input number is not even."
           | otherwise = head $ filter (\(x,y) -> x+y == n) primePair
                         where primePair = do x <- primes
                                              y <- dropWhile (<x) primes
                                              return (x,y)
                               primes = primesR 2 n

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList l u = map goldbach [lower,lower+2..upper]
                   where lower = l + l `mod` 2
                         upper = u - u `mod` 2

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' l u min = filter (\(n,m)-> n>min && m>min) $ goldbachList l u