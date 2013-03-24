pack :: (Eq a) => [a] -> [[a]]
pack x = foldr (\x y -> 
               if x==(head $ head y) 
               then (x:head y):tail y
               else [x]:y)  [[last x]] x