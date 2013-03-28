import System.Random

diff_select :: Int -> Int -> [Int]
diff_select c m = take c $ randomRs (1,m) (mkStdGen 1)