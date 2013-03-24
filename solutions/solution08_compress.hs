compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs@(y:_)) | x==y = compress xs
                      | otherwise = x : compress xs