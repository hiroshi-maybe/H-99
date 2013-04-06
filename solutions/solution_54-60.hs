import Debug.Trace

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- Problem 55

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [ Branch 'x' left right | lNodes <- bNum n, left <- cbalTree lNodes, right <- cbalTree (n-1-lNodes)]
             where bNum m = [base .. base+ (m-1) `mod` 2]
                            where base = (m-1) `div` 2

-- Problem 56

symmetric :: Tree a -> Bool
symmetric (Branch _ l r) = mirror l `strEq` r
                           where mirror Empty = Empty
                                 mirror (Branch x l r) = Branch x (mirror r) (mirror l)
                                 strEq (Branch _ al ar) (Branch _ bl br) = al `strEq` bl && ar `strEq` br
                                 strEq Empty Empty = True
                                 strEq _ _ = False

-- Problem 57

add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x (Branch y l r) = case x `compare` y of
                            GT -> Branch y l (add x r)
                            _  -> Branch y (add x l) r

construct :: (Ord a) => [a] -> Tree a
construct xs = foldl (\tree x -> add x tree) Empty xs

-- Problem 58

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = (filter symmetric).cbalTree

-- Problem 59

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h = [Branch x left right | (lh,rh) <- [(h-1,h-2),(h-2,h-1),(h-1,h-1)], left <- hbalTree x lh, right <- hbalTree x rh]
--hbalTree x h = [Branch x left right | (lh,rh) <- [(h-1,h-2)], left <- hbalTree x lh, right <- hbalTree x rh]

-- cbalTree with height (More balanced than problem 59)
cbalTreeHeight :: Int -> [Tree Char]
cbalTreeHeight h = concat $ map cbalTree [2^(h-1)..2^h-1]

-- Problem 60

minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)
maxHeight n = length $ takeWhile (<=n) $ minNodes

minNodes = 1:2:zipWith (\a b -> a+b+1) minNodes (tail minNodes)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = filter (\y -> countNodes y == n) $ concat $ map (hbalTree x) [minHeight n .. maxHeight n]
