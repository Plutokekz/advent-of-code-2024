import Data.Array
import System.IO

main :: IO ()
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let list = lines contents
  let width = length $ head list
  let height = length list
  let world = arrayFromStrings list
  putStrLn "Part Two:"
  let positions = [(0, -1), (1, 0), (0, 1), (-1, 0)]
  let newWolrd = move world (width, height) 0 positions (4, 6)-- (85, 48)--(4, 6)
  printArray newWolrd
  print $ count newWolrd 'X'
  let o =  move' (arrayFromStrings' list) (width, height) 0 positions (4, 6)-- (85, 48)--(4, 6)
  printArray' o
  print $ count' o 'O'
  hClose handle

count :: Array (Int, Int) Char -> Char -> Int
count world match = foldr (\c acc -> if c == match then acc + 1 else acc) 0 world

count' :: Array (Int, Int) (Char, (Int, Int)) -> Char -> Int
count' world match = foldr (\c acc -> if fst c == match then acc + 1 else acc) 0 world

printArray :: Array (Int, Int) Char -> IO ()
printArray arr =
  let ((minY, minX), (maxY, maxX)) = bounds arr
   in sequence_
        [ do
            sequence_ [putChar (arr ! (y, x)) | x <- [minX .. maxX]]
            putChar '\n'
          | y <- [minY .. maxY]
        ]

printArray' :: Array (Int, Int) (Char, (Int, Int)) -> IO ()
printArray' arr =
  let ((minY, minX), (maxY, maxX)) = bounds arr
   in sequence_
        [ do
            sequence_ [putChar $ fst (arr ! (y, x)) | x <- [minX .. maxX]]
            putChar '\n'
          | y <- [minY .. maxY]
        ]

arrayFromStrings :: [String] -> Array (Int, Int) Char
arrayFromStrings strings =
  array
    ((0, 0), (height - 1, width - 1))
    [ ((y, x), (strings !! y) !! x)
      | y <- [0 .. height - 1],
        x <- [0 .. width - 1]
    ]
  where
    height = length strings
    width = length (head strings)

arrayFromStrings' :: [String] -> Array (Int, Int) (Char, (Int, Int))
arrayFromStrings' strings =
  array
    ((0, 0), (height - 1, width - 1))
    [ ((y, x), ((strings !! y) !! x , (0,0)))
      | y <- [0 .. height - 1],
        x <- [0 .. width - 1]
    ]
  where
    height = length strings
    width = length (head strings)

move :: Array (Int, Int) Char -> (Int, Int) -> Int -> [(Int, Int)] -> (Int, Int) -> Array (Int, Int) Char
move world (width, height) directionIndex directions (x, y)
  | x >= width || x < 0 || y >= height || y < 0 = world
  | otherwise = case world ! (y, x) of
      '#' -> do
        let (dx, dy) = directions !! directionIndex
        let (oldX, oldY) = (x - dx, y - dy)
        let newDirectionIndex = mod (directionIndex + 1) (length directions)
        let (newDx, newDy) = directions !! newDirectionIndex
        let (newX, newY) = (oldX + newDx, oldY + newDy)
        move world (width, height) newDirectionIndex directions (newX, newY)
      _ -> do
        let (dx, dy) = directions !! directionIndex
        let (newX, newY) = (x + dx, y + dy)
        let newWolrd = world // [((y, x), 'X')]
        move newWolrd (width, height) directionIndex directions (newX, newY)

move' :: Array (Int, Int) (Char, (Int, Int)) -> (Int, Int) -> Int -> [(Int, Int)] -> (Int, Int) -> Array (Int, Int) (Char, (Int, Int))
move' world (width, height) directionIndex directions (x, y)
  | x >= width || x < 0 || y >= height || y < 0 = world
  | otherwise = case world ! (y, x) of
      ('#', _) -> do
        let (dx, dy) = directions !! directionIndex
        let (oldX, oldY) = (x - dx, y - dy)
        let newDirectionIndex = mod (directionIndex + 1) (length directions)
        let (newDx, newDy) = directions !! newDirectionIndex
        let (newX, newY) = (oldX + newDx, oldY + newDy)
        move' world (width, height) newDirectionIndex directions (newX, newY)
      (_, (oldDx, oldDy)) -> do
        let (newDx, newDy) = directions !! mod (directionIndex + 1) (length directions)
        let (dx, dy) = directions !! directionIndex
        let newWorld = if newDx == oldDx && newDy == oldDy then world // [((dy+y, dx+x), ('O', (0,0)))] else world
        let (newX, newY) = (x + dx, y + dy)
        let newNewWolrd = newWorld // [((y, x), (fst (newWorld ! (y,x)), (dx,dy)))]
        move' newNewWolrd (width, height) directionIndex directions (newX, newY)
