data ListItem a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect xs@(x:_) | len==1 = Single x : encodeDirect rest
                      | otherwise = Multiple len x : encodeDirect rest
                        where rest=dropWhile (==x) xs
                              len=length xs - length rest
                              