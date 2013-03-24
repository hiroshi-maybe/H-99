pack :: (Eq a) => [a] -> [[a]]
pack = foldr f [[]]
       where f x [[]] = [[x]]
             f x z@(y:ys) = if x==(head $ y) 
                          then (x:y):ys
                          else [x]:z