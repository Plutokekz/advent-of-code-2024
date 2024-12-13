-- (1,0)
-- (5,4)
-- (4,5)
-- (4,3)
-- (0,3)
-- [(5,4),(4,5),(4,3),(0,3),(1,0),(5,4),(4,5),(4,3),(0,3),(1,0),(5,4),(4,5),(4,3),(0,3),(1,0),(5,4),(4,5),(4,3),(0,3),(1,0)]

import Data.Char
import Data.Set qualified as Set
import System.IO

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let world = map (\line -> [digitToInt x | x <- line]) (lines contents)
  let width = length (head world)
  let height = length world
  let starts = startPositions world (width, height)
  let result = map (move' world (width, height)) starts
  putStrLn "Part One:"
  print $ sum $ map countUnique result

  putStrLn "Part Two:"
  print $ sum $ map length result
  hClose handle

countUnique :: [(Int, Int)] -> Int
countUnique = Set.size . Set.fromList

startPositions :: [[Int]] -> (Int, Int) -> [(Int, Int)]
startPositions world (width, height) = concat [[(x, y) | x <- [0 .. (width - 1)], (world !! y !! x) == 0] | y <- [0 .. (height - 1)]]

inBound :: (Int, Int) -> (Int, Int) -> Bool
inBound (width, height) (x, y) =
  width > x && x >= 0 && height > y && y >= 0

goingUp :: [[Int]] -> Int -> (Int, Int) -> Bool
goingUp world value (x, y) = ((world !! y) !! x) - value == 1

move' :: [[Int]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
move' world size (x, y)
  | ((world !! y) !! x) == 9 = [(x, y)]
  | otherwise = do
      -- print world
      -- calculate all valid neighbors that are in the world for the current position
      let neighbors = filter (inBound size) $ map (\(dx, dy) -> (x + dx, y + dy)) [(1, 0), (0, 1), (-1, 0), (0, -1)]
      -- print neighbors
      -- get all valid postiions that are going up the hill
      let allowed = filter (goingUp world ((world !! y) !! x)) neighbors
      -- print allowed
      -- no tailrecursion here, we need to summ all results from all possible paths
      concat [move' world size newPos | newPos <- allowed]