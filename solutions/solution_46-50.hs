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
xor' a b = a /= not' b

mpl' :: Bool -> Bool -> Bool
mpl' = (not'.).or'

equ' :: Bool -> Bool -> Bool
equ' = (==)

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ (show $ f x y)
                                | x<-[True, False], y<-[True, False]]
