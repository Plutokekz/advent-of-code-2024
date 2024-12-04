import System.IO

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = lines contents
  let width = length $ head list
  let height = length list
  putStrLn "Part Two:"
  print $ searchMap list (width, height) list 0 0 searchRow
  putStrLn "Part Two:"
  print $ searchMap list (width, height) list 0 0 searchRow'
  hClose handle

searchMap :: [String] -> (Int, Int) -> [String] -> Int -> Int -> ([String] -> (Int, Int) -> (Int, Int) -> String -> Int -> Int) -> Int
searchMap _ _ [] _ acc _ = acc
searchMap wordMap (width, height) (row : rest) y acc searchRowFunction = do
  let newAcc = acc + searchRowFunction wordMap (width, height) (0, y) row 0
  searchMap wordMap (width, height) rest (y + 1) newAcc searchRowFunction

searchRow :: [String] -> (Int, Int) -> (Int, Int) -> String -> Int -> Int
searchRow wordMap (width, height) (x, y) [] acc = acc
searchRow wordMap (width, height) (x, y) (column : row) acc
  | column == 'X' = do
      -- map the partial search function over all neighbors and search for MAS
      let result = map (search wordMap (width, height) (x, y) "MAS") [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]
      -- count how often a match occurred [True, True, False, ...] in the map result
      let newAcc = acc + length [n | n <- result, n]
      -- recursive call for next element
      searchRow wordMap (width, height) (x + 1, y) row newAcc
  | otherwise = searchRow wordMap (width, height) (x + 1, y) row acc

searchRow' :: [String] -> (Int, Int) -> (Int, Int) -> String -> Int -> Int
searchRow' wordMap (width, height) (x, y) [] acc = acc
searchRow' wordMap (width, height) (x, y) (column : row) acc
  | column == 'A' = do
      -- If an A was matched we check for all possible patterns
      let newAcc = if search' wordMap (width, height) (x, y) then acc + 1 else acc
      -- recursive call for next element
      searchRow' wordMap (width, height) (x + 1, y) row newAcc
  | otherwise = searchRow' wordMap (width, height) (x + 1, y) row acc

search :: [String] -> (Int, Int) -> (Int, Int) -> String -> (Int, Int) -> Bool
search _ _ _ [] _ = True
search wordMap (width, height) (x, y) (match : rest) (dx, dy) = do
  let newX = x + dx
  let newY = y + dy
  -- check if new x and y are in bound of the map
  not (newX < 0 || newX >= width || newY < 0 || newY >= height)
    -- match the character of the position newX and newY with the current excepted on
    && ((wordMap !! newY) !! newX) == match
    -- Recursive call to match the next character
    && search wordMap (width, height) (newX, newY) rest (dx, dy)

isValidPosition :: (Int, Int) -> (Int, Int) -> Bool
isValidPosition (width, height) (x, y) = x >= 0 && x < width && y >= 0 && y < height

check :: [String] -> String -> [(Int, Int)] -> Bool
check _ [] [] = True
-- check if the current char at x, y matches the expected char, if yes, check recursively for the next one
check wordMap (m : rest) ((x, y) : neighbors) = wordMap !! y !! x == m && check wordMap rest neighbors

search' :: [String] -> (Int, Int) -> (Int, Int) -> Bool
search' wordMap (width, height) (x, y) = do
  let neighbors = map (\(dx, dy) -> (x + dx, y + dy)) [(-1, -1), (1, -1), (1, 1), (-1, 1)]
  let areAllValid = all (isValidPosition (width, height)) neighbors
  -- check if all neighbors are valid, then check if one of the 4 possible patters appear
  areAllValid && (check wordMap "SSMM" neighbors || check wordMap "MMSS" neighbors || check wordMap "MSSM" neighbors || check wordMap "SMMS" neighbors)