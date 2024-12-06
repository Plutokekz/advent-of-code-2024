import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Set (Set, empty, insert, member)
import Data.Set qualified as Set
import System.IO

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let (rawRules, input) = splitInput contents
  let rules = toRules rawRules IntMap.empty
  putStrLn "Part One:"
  print $ checkPages input rules 0
  putStrLn "Part Two:"
  print $ checkPages' input rules 0
  hClose handle

toRules :: [[Int]] -> IntMap (Set Int) -> IntMap (Set Int)
-- create a map with a value as the key and a set with the values that need to come after the key value as value.
toRules [] rules = rules
toRules (x : xs) rules = do
  if IntMap.member (x !! 0) rules
    then
      toRules xs (IntMap.adjust (insert (x !! 1)) (x !! 0) rules)
    else
      toRules xs (IntMap.insert (x !! 0) (insert (x !! 1) empty) rules)

checkPages :: [[Int]] -> IntMap (Set Int) -> Int -> Int
checkPages [] _ acc = acc
checkPages (r : ow) rules acc
  -- go through each row and check if the pages are ordered; if so, take the middle value and add it recursivly
  | checkRow r empty rules = checkPages ow rules (acc + middle r)
  | otherwise = checkPages ow rules acc

badSort :: [Int] -> IntMap (Set Int) -> [Int]
-- badSort with default values
badSort = badSort' [] empty

badSort' :: [Int] -> Set Int -> [Int] -> IntMap (Set Int) -> [Int]
badSort' acc _ [] _ = acc
badSort' acc visited (i : nput) rules = do
  -- sort to not ordered pages
  -- If a value is not in order, put it at the beginning of the list and start the sorting all over.
  case IntMap.lookup i rules of
    (Just rule) ->
      if check visited rule
        then
          badSort' (acc ++ [i]) (insert i visited) nput rules
        else
          badSort' [] empty (i : acc ++ nput) rules
    Nothing -> badSort' (acc ++ [i]) (insert i visited) nput rules

checkPages' :: [[Int]] -> IntMap (Set Int) -> Int -> Int
checkPages' [] _ acc = acc
checkPages' (r : ow) rules acc
  -- go through all rows and sort the unordered ones, and then take the middle value.
  | not (checkRow r empty rules) = checkPages' ow rules (acc + middle (badSort r rules))
  | otherwise = checkPages' ow rules acc

middle :: [Int] -> Int
middle [] = 0
middle xs = xs !! div (length xs) 2

checkRow :: [Int] -> Set Int -> IntMap (Set Int) -> Bool
-- if all page numbers are checked, everything is fine.
checkRow [] _ _ = True
checkRow (i : nput) visited rules = do
  -- check if a rule for the current page number exists.
  case IntMap.lookup i rules of
    -- if a rule exists, check if the rule is true, then check recursively for the rest.
    (Just rule) -> check visited rule && checkRow nput (insert i visited) rules
    -- if no rule exists, skip the page number and go on with the next one
    Nothing -> checkRow nput (insert i visited) rules

check :: Set Int -> Set Int -> Bool
check before rule = Set.disjoint rule before

-- parse input util functions
toInt :: [String] -> [Int]
toInt = map (\x -> read x :: Int)

splitInput :: String -> ([[Int]], [[Int]])
splitInput input = do
  let l = lines input
  let top = map (toInt . split '|') (takeWhile (/= "") l)
  let bottom = map (toInt . split ',') (drop (length top + 1) l)
  (top, bottom)

split :: Char -> String -> [String]
split match word = split' match word ""

split' :: Char -> String -> String -> [String]
split' _ [] acc = [acc]
split' match (w : ord) acc
  | w == match = acc : split' match ord ""
  | otherwise = split' match ord (acc ++ [w])