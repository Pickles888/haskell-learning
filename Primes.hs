import System.Environment qualified as Env

main :: IO ()
main = do
  num <- read . head <$> Env.getArgs
  mapM_ (putStrLn . toStr)
    $ take num
      . isAddTwo
    $ getPair primes

toStr :: (Show a) => (a, a) -> String
toStr (a, b) = show a ++ ", " ++ show b

getPair :: [a] -> [(a, a)]
getPair [] = []
getPair [x] = []
getPair (x : xs) = (x, head xs) : getPair xs

isAddTwo :: (Eq a, Num a) => [(a, a)] -> [(a, a)]
isAddTwo = filter (\(a, b) -> a + 2 == b)

-- stolen from haskell.org -_-
primes = filterPrime [2 ..]
  where
    filterPrime (p : xs) =
      p : filterPrime [x | x <- xs, x `mod` p /= 0]
