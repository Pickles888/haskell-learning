import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)

type Cell = Bool

type Board = [[Cell]]

main :: IO ()
main = gameLoop glider

gameLoop :: Board -> IO ()
gameLoop board = do
  mapM_ (putStrLn . concat) $ displayBoard board
  threadDelay 300000
  putStrLn "" >> putStrLn "---" >> putStrLn ""
  gameLoop $ update board

displayBoard :: Board -> [[String]]
displayBoard = map boolToChar
  where
    boolToChar = map (\a -> if a then "[]" else "* ")

update :: Board -> Board
update board =
  let size = length board
   in [[parseNeighbors (board !! y !! x) (getNeighbors board x y) | x <- [0 .. size - 1]] | y <- [0 .. size - 1]]

getNeighbors :: Board -> Int -> Int -> Int
getNeighbors board x y =
  let wrapCoord a
        | a >= size = a - size
        | a < 0 = size + a
        | otherwise = a
        where
          size = length board
   in sum
        [1 | nx <- map wrapCoord [x - 1 .. x + 1], ny <- map wrapCoord [y - 1 .. y + 1], (nx, ny) /= (x, y), board !! ny !! nx]

parseNeighbors :: Bool -> Int -> Bool
parseNeighbors True num
  | num < 2 = False
  | num > 3 = False
  | otherwise = True
parseNeighbors False num
  | num == 3 = True
  | otherwise = False

glider :: [[Bool]]
glider =
  [ [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, True, False, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, False, True, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, True, True, True, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, True, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, True, False, False, False, False],
    [False, False, False, False, False, False, False, False, True, False, False, True, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, True, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, True, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, True, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False]
  ]
