main :: IO ()
main = run [] []

data Command = Add String | Remove Int | Complete Int | Uncomplete Int | Exit | None

run :: [Int] -> [(Int, String)] -> IO ()
run c xs = do
  putStr $ display (complete c) xs
  command <- parse . words <$> getLine
  case command of
    Add s -> run c $ add s xs
    Remove x -> run c $ filter ((/=) x . fst) xs
    Complete x -> run (c ++ [x]) xs
    Uncomplete x -> run (filter (/= x) c) xs
    Exit -> mempty
    None -> run c xs

add :: String -> [(Int, String)] -> [(Int, String)]
add s [] = [(1, s)]
add s xs = xs ++ [(fst (last xs) + 1, s)]

parse :: [String] -> Command
parse [] = None
parse [x] = case x of
  "exit" -> Exit
  _ -> None
parse (a : b : xs) =
  let numB = read b
   in case a of
        "add" -> Add (unwords (b : xs))
        "rm" -> Remove numB
        "c" -> Complete numB
        "uc" -> Uncomplete numB
        _ -> None

display :: (Int -> Bool) -> [(Int, String)] -> String
display f = unlines . map (\(a, b) -> show a ++ showC (f a) ++ b)
  where
    showC bool = if bool then " [X] " else " [ ] "

complete :: [Int] -> (Int -> Bool)
complete a x = x `elem` a
