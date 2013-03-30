isPrime :: Int -> Bool
isPrime n = null $ filter (\x -> n `mod` x==0) [2..squareRoot n]

squareRoot :: Int -> Int
squareRoot = floor . sqrt . fromIntegral