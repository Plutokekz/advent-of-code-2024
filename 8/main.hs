import Data.Array
import Data.Set (Set, empty, insert, toList, fromList)
import Data.Set qualified as Set
import System.IO
import qualified Data.Maybe

type Type = Char

type Position = (Int, Int)

type Direction = (Int, Int)

type Antenna = (Position, Type)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = lines contents
  let ((width, height), world) = arrayFromStrings list
  print (width, height)
  let antennas = toList $ uniqAntennas empty world (0, 0) (width, height)
  let result = toList $ fromList $ concatMap (\a -> combinations (filter2D world (== a)) [] align) antennas
  let validAntennas = filter (\(y, x) -> x >= 0 && y >= 0 && x < width && y < height) result
  print validAntennas
  print $ length validAntennas
  let result = toList $ fromList $ concatMap (\a -> combinations (filter2D world (== a)) [] align') antennas
  print $ length $ filter (\(y, x) -> x >= 0 && y >= 0 && x < width && y < height) result
  hClose handle

uniqAntennas :: Set Type -> Array (Int, Int) Type -> (Int, Int) -> (Int, Int) -> Set Type
uniqAntennas acc field (x, y) (width, height)
  | y == height = acc
  | x == width = uniqAntennas acc field (0, y + 1) (width, height)
  | otherwise =
      uniqAntennas
        ( case field ! (y, x) of
            '.' -> acc
            x -> insert x acc
        )
        field
        (x + 1, y)
        (width, height)

combinations :: [Antenna] -> [Position] -> (Antenna -> Antenna -> Maybe [Position]) -> [Position]
combinations [x] acc _ = acc
combinations [] acc _ = acc
combinations (x : xs) acc f = do
  let results = map (f x) xs
  combinations xs (acc ++ concat (Data.Maybe.catMaybes results)) f

direction :: Position -> Position -> Direction
direction (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

scalar :: Direction -> Int -> Direction
scalar (dx, dy) s = (dx * s, dy * s)

add :: Position -> Direction -> Position
add (x, y) (dx, dy) = (x + dx, y + dy)

sub :: Position -> Direction -> Position
sub (x, y) (dx, dy) = (x - dx, y - dy)

align' :: Antenna -> Antenna -> Maybe [Position]
align' (p1, t1) (p2, t2)
  | t1 /= t2 = Nothing
  | otherwise =
      if add p1 dir == p2
        then
          Just ([add p1 $ scalar dir x | x <- [0..50]] ++ [add p1 $ scalar dir (-x) | x <- [0..50]])
        else
          Nothing
  where
    dir = direction p1 p2

align :: Antenna -> Antenna -> Maybe [Position]
align (p1, t1) (p2, t2)
  | t1 /= t2 = Nothing
  | otherwise =
      if add p1 dir == p2
        then
          Just [add p1 $ scalar dir 2, sub p1 dir]
        else
          Nothing
  where
    dir = direction p1 p2

filter2D :: Array (Int, Int) Type -> (Type -> Bool) -> [Antenna]
filter2D arr pred = [((i, j), val) | (i, j) <- range (bounds arr), let val = arr ! (i, j), pred val]

arrayFromStrings :: [String] -> ((Int, Int), Array (Int, Int) Type)
arrayFromStrings strings =
  ( (width, height),
    array
      ((0, 0), (height - 1, width - 1))
      [ ((y, x), strings !! y !! x)
        | y <- [0 .. height - 1],
          x <- [0 .. width - 1]
      ]
  )
  where
    height = length strings
    width = length (head strings)