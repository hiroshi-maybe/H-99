import Data.List (group)

-- Copy of solution 35.
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = (factor:) $ primeFactors $ n `div` factor
                 where isFactor = (==0).(mod n)
                       factor = head $ filter isFactor [2..n]

-- Copy of solution 36
prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult = map (\xs -> (head xs,length xs)) . group . primeFactors

totient :: Int -> Int
totient n = foldl (\acc (val,len) -> acc * (val-1) * val ^ (len-1) ) 1 $  prime_factors_mult n
