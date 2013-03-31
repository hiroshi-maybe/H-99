-- Copy of solution 33.
coprime :: (Eq a, Integral a) => a -> a -> Bool
coprime x y = (==1) $ gcd x y

totient :: Int -> Int
totient n = length $ filter id $ map (coprime n) [1..n]