import Data.Char
import System.IO

data Memory = File | Free deriving (Eq, Show)

main :: IO ()
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let list = head (lines contents)
  let (ex, file) = expand [digitToInt x | x <- list]
  print file
  let re = reduce ex
  -- print re
  print $ checksum re
  hClose handle

expand :: [Int] -> ([String], Int)
expand = expand' [""] File 0

expand' :: [String] -> Memory -> Int -> [Int] -> ([String], Int)
expand' acc memory fileIndex [] = (acc, fileIndex - 1)
expand' acc memory fileIndex (x : xs)
  | memory == File = expand' (acc ++ [show fileIndex | n <- [1 .. x]]) Free (fileIndex + 1) xs
  | memory == Free = expand' (acc ++ ["." | n <- [1 .. x]]) File fileIndex xs

reduce :: [String] -> [String]
reduce x = reduce' [""] 0 (length x) x (reverse x)

reduce' :: [String] -> Int -> Int -> [String] -> [String] -> [String]
reduce' acc f b (x : xs) (s : sx)
  | f == b = acc
  | s == "." = reduce' acc f (b - 1) (x : xs) sx
  | x == "." = reduce' (acc ++ [s]) (f + 1) (b - 1) xs sx
  | otherwise = reduce' (acc ++ [x]) (f + 1) b xs (s : sx)

checksum :: [String] -> Int
checksum = checksum' 0 0

checksum' :: Int -> Int -> [String] -> Int
checksum' acc _ [] = acc
checksum' acc index (x : xs) = checksum' (acc + ((read x ::Int) * index)) (index + 1) xs