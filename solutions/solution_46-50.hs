-- for Problem 49
import Control.Monad (replicateM)

-- Problem 46.

not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' = (not'.).and'

nor' :: Bool -> Bool -> Bool
nor' = (not'.).or'

xor' :: Bool -> Bool -> Bool
xor' a b = a == not' b

mpl' :: Bool -> Bool -> Bool
mpl' = (not'.).or'

equ' :: Bool -> Bool -> Bool
equ' = (==)

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ (show $ f x y)
                                | x<-[True, False], y<-[True, False]]

-- Problem 47.

infixl 4 `or'`
infixl 6 `and'`

-- Problem 48.

infixl 3 `equ'`

truthGen :: Int -> [[Bool]]
truthGen 0 = [[]]
truthGen n = do x <- [True, False]
                y <- truthGen (n-1)
                return (x:y)

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [ strTruth xs xs | xs <- truthGen n]
             where strTruth (x:xs) params = show x ++ " " ++ (strTruth xs params)
                   strTruth [] params = show $ f params

-- Problem 49.

gray :: Int -> [String]
gray n = map (toString.genGray) $ replicateM n [False,True]
         where shiftR xs = False : init xs
               genGray xs = genGray' xs $ shiftR xs
                            where genGray' [] _ = []
                                  genGray' (x:xs) (y:ys) = (x `xor'` y):(genGray' xs ys)
               toString xs = map (\x -> if x then '1' else '0') xs