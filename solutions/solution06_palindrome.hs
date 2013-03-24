isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "empty list."
isPalindrome [_] = True
isPalindrome (x:xs) = (x==l) && (isPalindrome $ init xs)
                      where l = last xs
