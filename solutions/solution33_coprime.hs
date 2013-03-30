coprime :: (Eq a, Integral a) => a -> a -> Bool
coprime x y = (==1) $ gcd x y