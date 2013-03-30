isPrime :: Int -> Bool
isPrime n = null $ filter (\x -> n `mod` x==0) [2..squareRoot]
            where squareRoot = floor.sqrt.fromIntegral $ n
