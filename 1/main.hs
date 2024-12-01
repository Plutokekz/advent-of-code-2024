import System.IO
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Maybe

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let list = map words $ lines contents
        let lists = splitList list [] []
        let distances = uncurry distance lists []
        putStrLn "Part One:"
        print $ sum distances
        let coutnmap = count (snd lists) IntMap.empty
        let similarties = similarity (fst lists) coutnmap []
        putStrLn "Part Two:"
        print $ sum similarties
        hClose handle

splitList :: [[String]] -> [Int]-> [Int] -> ([Int], [Int])
splitList [] acc1 acc2 = (sort acc1, sort acc2)
splitList (x:xs) acc1 acc2 = splitList xs (acc1++[read (x!!0) :: Int]) (acc2++[read (x!!1) :: Int])

distance :: [Int] -> [Int] -> [Int] -> [Int]
distance [] [] acc = acc
distance (x:xs) (y:ys) acc = distance xs ys (acc ++ [abs (x - y)])


count :: [Int] -> IntMap Int -> IntMap Int
count [] acc = acc
count (x:xs) acc = do
    let acc2 = if IntMap.member x acc
               then IntMap.adjust (1+) x acc
               else IntMap.insert x 1 acc
    count xs acc2


similarity :: [Int] -> IntMap Int -> [Int] -> [Int]
similarity [] _ acc = acc
similarity (x:xs) countmap acc = do
    let multiplayer = Data.Maybe.fromMaybe 0 (IntMap.lookup x countmap)
    similarity xs countmap (acc ++ [x * multiplayer])



