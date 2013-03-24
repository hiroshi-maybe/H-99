myButLast :: [a]->a
myButLast [] = error "empty list."
myButLast [_] = error "needing over 2 elements."
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
