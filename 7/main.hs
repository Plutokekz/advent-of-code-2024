import System.IO

-- define a type that represents our add and multiply function two Integers as input and one as result.
type BinaryOperation = Int -> Int -> Int

nLength :: Int -> Int
nLength 0 = 1
nLength n = 1 + numberLength' (div n 10)

numberLength' :: Int -> Int
numberLength' 0 = 0
numberLength' n = 1 + numberLength' (div n 10)

(|||) :: Int -> Int -> Int
(|||) a b = a * (10 ^ nLength b) + b

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = map parseLine $ lines contents
  let functions = [(+), (*)]
  putStrLn "Part One:"
  -- summing the expected values if the brute force function finds a possible result
  -- we calculate the Cartesian product of the available function and the value length -1
  -- then we calculate all possible results and check if one of them is the expected result
  -- if a valid combination is found, we add the expected value to the accumulator and fold down all inputs
  print $ foldr (\(expected, values) acc -> if bruteForce (cartesianProduct functions (length values - 1)) (expected, values) then acc + expected else acc) 0 input
  let functionsPartTwo = [(+), (*), (|||)]
  putStrLn "Part Two:"
  -- This takes some time to run ^^
  print $ foldr (\(expected, values) acc -> if bruteForce (cartesianProduct functionsPartTwo (length values - 1)) (expected, values) then acc + expected else acc) 0 input
  hClose handle

parseLine :: String -> (Int, [Int])
parseLine line = do
  let expected = takeWhile (/= ':') line
  let values = map (\x -> read x :: Int) (words (drop (length expected + 1) line))
  (read expected :: Int, values)

bruteForce :: [[BinaryOperation]] -> (Int, [Int]) -> Bool
bruteForce [] _ = False
bruteForce (x : xs) (expected, values) = (expected == calculate values x) || bruteForce xs (expected, values)

calculate :: [Int] -> [BinaryOperation] -> Int
-- if all function were applied, the result is on top of the stack
calculate [x] [] = x
-- stack-based evaluation apply function two first 2 elements from list (stack) then put the result back on top of the stack
calculate (x : y : numbers) (f : functions) = calculate (f x y : numbers) functions

cartesianProduct :: [BinaryOperation] -> Int -> [[BinaryOperation]]
cartesianProduct list len = sequence (replicate len list)