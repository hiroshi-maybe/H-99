myGCD :: Int -> Int -> Int
myGCD 0 y = y
myGCD x y | n > m = myGCD (n `mod` m) m
          | otherwise = myGCD (m `mod` n) n
          where n = abs x
                m = abs y