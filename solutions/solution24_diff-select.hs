import System.Random

diff_select :: Int -> Int -> IO [Int]
diff_select c m = diff_select' c [1..m]
                  where diff_select' :: Int -> [Int] -> IO [Int]
                        diff_select' 0 _ = return []
                        diff_select' c xs = do rand <- randomRIO (0,length xs -1)
                                               let combed = take rand xs ++ drop (rand+1) xs
                                               rest <- diff_select' (c-1) combed
                                               return (xs!!rand : rest)