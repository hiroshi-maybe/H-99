import Debug.Trace

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- Problem 55

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [ Branch 'x' left right | lNodes <- bNum n, left <- cbalTree lNodes, right <- cbalTree (n-1-lNodes)]
             where bNum m = [base .. base+ (m-1) `mod` 2]
                            where base = (m-1) `div` 2

-- Problem 56

symmetric :: Tree Char -> Bool
symmetric (Branch _ l r) = mirror l `strEq` r
                           where mirror Empty = Empty
                                 mirror (Branch x l r) = Branch x (mirror r) (mirror l)
                                 strEq (Branch _ al ar) (Branch _ bl br) = al `strEq` bl && ar `strEq` br
                                 strEq Empty Empty = True
                                 strEq _ _ = False
