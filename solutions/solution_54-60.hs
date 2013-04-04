import Debug.Trace

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [ Branch 'x' left right | lNodes <- bNum n, left <- cbalTree lNodes, right <- cbalTree (n-1-lNodes)]
             where bNum m | odd m = [base]
                          | otherwise = [base, base+1]
                          where base = (m-1) `div` 2
