-- Copy from soluction09
pack :: (Eq a) => [a] -> [[a]]
pack = foldr f [[]]
       where f x [[]] = [[x]]
             f x z@(y:ys) = if x==(head $ y) 
                          then (x:y):ys
                          else [x]:z

encode :: (Eq a) => [a] -> [(Int,a)]
encode x = map (\y -> (length y, head y)) $ pack x