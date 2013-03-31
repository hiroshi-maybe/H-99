-- If change start index from 2 to previous factor, performance will get better.
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = (factor:) $ primeFactors $ n `div` factor
                 where isFactor = (==0).(mod n)
                       factor = head $ filter isFactor [2..n]