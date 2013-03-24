-- Copy from soluction09
pack :: (Eq a) => [a] -> [[a]]
pack = foldr f [[]]
       where f x [[]] = [[x]]
             f x z@(y:ys) = if x==(head $ y) 
                          then (x:y):ys
                          else [x]:z

encode :: (Eq a) => [a] -> [(Int,a)]
encode x = map (\y -> (length y, head y)) $ pack x

data ListItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified x = map (\(len,val) ->
                       if len==1
                       then Single val
                       else Multiple len val) $ encode x