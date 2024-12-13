import Data.Array
import System.IO

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let ((width, height), world) = arrayFromStrings $ lines contents
  print world
  hClose handle

arrayFromStrings :: [String] -> ((Int, Int), Array (Int, Int) (Char, Bool))
arrayFromStrings strings =
  ( (width, height),
    array
      ((0, 0), (height - 1, width - 1))
      [ ((y, x), (strings !! y !! x, False))
        | y <- [0 .. height - 1],
          x <- [0 .. width - 1]
      ]
  )
  where
    height = length strings
    width = length (head strings)

inBound :: (Int, Int) -> (Int, Int) -> Bool
inBound (width, height) (x, y) =
  width > x && x >= 0 && height > y && y >= 0

checkCharAndVisited :: Array (Int, Int) (Char, Bool) -> Char -> (Int, Int) -> Bool
checkCharAndVisited world match (x, y) = world ! (y, x) == (match, False)

findTile :: Array (Int, Int) (Char, Bool) -> Char -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
findTile world match size (x, y) = do
  -- TODO add visited to world
  let neighbors = filter (inBound size) $ map (\(dx, dy) -> (x + dx, y + dy)) [(1, 0), (0, 1), (-1, 0), (0, -1)]
  -- print neighbors
  -- get all valid postiions that are going up the hill
  let allowed = filter (checkCharAndVisited world match) neighbors
  -- print allowed
  -- no tailrecursion here, we need to summ all results from all possible paths
  -- todo add current positition to list to return the howl tile
  concat [findTile world size newPos | newPos <- allowed]