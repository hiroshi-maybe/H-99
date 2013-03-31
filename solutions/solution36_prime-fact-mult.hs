import Data.List (group)

-- Copy of solution 35.
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = (factor:) $ primeFactors $ n `div` factor
                 where isFactor = (==0).(mod n)
                       factor = head $ filter isFactor [2..n]

prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult = map (\xs -> (head xs,length xs)) . group . primeFactors
