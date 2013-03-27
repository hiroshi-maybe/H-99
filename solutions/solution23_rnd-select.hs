import System.Random
import Control.Monad (replicateM)
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = replicateM n mrand
                  where mrand = do ran <- randomRIO (0,length xs -1)
                                   return (xs!!ran)
-- Returns value which is not monad
-- rnd_select :: [a] -> Int -> [a]
-- rnd_select xs n = map (xs!!) $ take n $ randomRs (0,10) (mkStdGen 0)
                
           
