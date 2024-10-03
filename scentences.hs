import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . getScentences

getScentences :: [String] -> Int
getScentences = length . filter (`elem` ".?!") . map last
