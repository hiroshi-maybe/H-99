import System.Random

rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu xs = do rnd <- randomRIO (0, length xs -1)
                  let combed = let (ys,z:zs) = splitAt rnd xs in ys++zs
                  rest <- rnd_permu combed
                  return ((xs!!rnd):rest)