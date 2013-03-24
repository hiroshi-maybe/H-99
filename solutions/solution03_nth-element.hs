elementAt :: [a]->Int->a
elementAt (x:_) 1 = x
elementAt (_:xs) n | n<1 = error "Second arg should be over 1."
                   | otherwise = elementAt xs $ n-1
