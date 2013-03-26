rotate :: [a] -> Int -> [a]
rotate xs n = drop border xs ++ take border xs 
              where len = length xs
                    border = (len+n) `mod` len