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

minNodes = 1:2:zipWith ((+).(+1)) minNodes (tail minNodes)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = filter ((==n).countNodes) $ concatMap (hbalTree x) [minHeight n .. maxHeight n]

-- Problem 61

countLeaves :: Tree a -> Int
countLeaves (Branch _ Empty Empty) = 1
countLeaves Empty = 0
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- Problem 61A

leaves :: Tree a -> [a]
leaves (Branch x Empty Empty) = [x]
leaves Empty = []
leaves (Branch _ l r) = leaves l ++ leaves r

-- Problem 62

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x: internals l ++ internals r

-- Problem 62B

atLevel :: Tree a -> Int -> [a]
atLevel Empty _          = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch x l r) n = atLevel l (n-1) ++ atLevel r (n-1)

-- Problem 63

completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = Branch 'x' (completeBinaryTree l) (completeBinaryTree r)
                       where r = (n-1) `div` 2
                             l = r + (n-1) `mod` 2

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = fst $ iscbt' t 1

iscbt' :: Tree a -> Int -> (Bool,(Int,Int))
iscbt' Empty n = (True,(n,n))
iscbt' (Branch _ l r) n = (lchk&&rchk&&cbtCheck, (depthmin, depthmax))
                          where (lchk, (ldepthmin, ldepthmax)) = iscbt' l (n+1)
                                (rchk, (rdepthmin, rdepthmax)) = iscbt' r (n+1)
                                depthmin = min ldepthmin rdepthmin
                                depthmax = max ldepthmax rdepthmax
                                cbtCheck | ldepthmax < rdepthmin = False
                                         | depthmax - depthmin > 1 = False
                                         | otherwise = True

-- Problem 64

inorderTraversal :: Tree a -> Int -> Int -> Tree (a,Int,Int,Int)
inorderTraversal Empty _ _ = Empty
inorderTraversal (Branch x l r) n h = Branch (x, (lindex+1), rindex, h+1) left right
                                      where nodeIndex node index = case inorderTraversal node index (h+1) of
                                                                        node'@(Branch (x,_,m,_) _ _) -> (node',m)
                                                                        Empty -> (Empty,index)
                                            (left,lindex) = nodeIndex l n
                                            (right,rindex) = nodeIndex r (lindex+1)

tformat :: Tree (a,Int,Int,Int) -> Tree (a,(Int,Int))
tformat Empty = Empty
tformat (Branch (v,x,_,y) l r) = Branch (v,(x,y)) (tformat l) (tformat r)

layout :: Tree a -> Tree (a,(Int,Int))
layout t = tformat traversal
           where traversal = inorderTraversal t 0 0