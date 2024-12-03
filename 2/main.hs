import Data.List
import System.IO

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = map (map read . words) (lines contents) :: [[Int]]
  let extendedList = produce list
  putStrLn "Part One:"
  print $ checkList list 0
  putStrLn "Part Two:"
  print $ checkList' extendedList 0
  hClose handle

produce :: [[Int]] -> [[[Int]]]
produce = map (\x -> x : rm x)

checkList :: [[Int]] -> Int -> Int
checkList [] acc = acc
checkList (x : xs) acc = do
  let cmp = incrdecr x
  let nexacc = (if check x cmp then 1 else 0) + acc
  checkList xs nexacc

checkList' :: [[[Int]]] -> Int -> Int
checkList' [] acc = acc
checkList' (x : xs) acc = do
  let valid
        | multiCheck x = 1
        | otherwise = 0
  checkList' xs (acc + valid)

rm :: [Int] -> [[Int]]
rm [] = []
rm (x : xs) = xs : map (x :) (rm xs)

multiCheck :: [[Int]] -> Bool
multiCheck [] = False
multiCheck (x : xs) = do
  let cmp = incrdecr x
  check x cmp || multiCheck xs

incrdecr :: [Int] -> (Int -> Int -> Bool)
incrdecr (x : y : ys)
  | x > y = (>)
  | x < y = (<)
  | x == y = incrdecr (x : ys)

check :: [Int] -> (Int -> Int -> Bool) -> Bool
check [_] _ = True
check (x : y : xs) cmp = do
  let diff = abs $ x - y
  not (diff < 1 || diff > 3) && cmp x y && check (y : xs) cmp