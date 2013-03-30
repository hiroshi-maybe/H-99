myGCD :: Int -> Int -> Int
myGCD x 0 = x
myGCD 0 y = y
myGCD x y = gcd (x `mod` y) y