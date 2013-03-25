data ListItem a = Single a | Multiple Int a deriving (Show)
decodeModified :: [ListItem a] -> [a]
decodeModified  = foldl decodeListItem []
                  where decodeListItem xs (Single x) = xs++[x]
                        decodeListItem xs (Multiple n x) = xs++replicate n x
