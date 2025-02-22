data Stream a = Cons a (Stream a)

toStream :: [a] -> Stream a
toStream (x : xs) = Cons x $ toStream xs

natsFrom :: Integer -> Stream Integer
natsFrom n = Cons (toInteger n) $ natsFrom (n + 1)

nats :: Stream Integer
nats = natsFrom 0

takeStream :: Integer -> Stream a -> [a]
takeStream 0 _ = []
takeStream n (Cons x xs) = x : takeStream (n - 1) xs

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (Cons x xs) = Cons (f x) $ mapStream f xs

mapStreamM_ :: (Monad m) => (a -> m ()) -> Integer -> Stream a -> m ()
mapStreamM_ f a = sequence_ . takeStream a . mapStream f

filterStream p (Cons x xs)
  | p x = Cons x (filterStream p xs)
  | otherwise = filterStream p xs

main :: IO ()
main = mapStreamM_ (print . (+ 1)) 100 nats
