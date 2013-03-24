myLast :: [a]->a
myLast [] = error "empty list."
myLast [x] = x
myLast (_:xs) = myLast xs

-- myLast xs = last xs
