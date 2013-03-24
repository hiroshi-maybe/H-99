myLast :: [a]->a
myLast [x] = x
myLast (x:xs) = myLast xs

-- myLast xs = last xs
