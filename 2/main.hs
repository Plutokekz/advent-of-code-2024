import System.IO
import Data.List

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let list = map (map read . words) (lines contents) :: [[Int]]
        print list
        print $ checkList list 0 check'
        print $ check' [7,6,4,2,1] (>)
        print $ check' [1,2,7,8,9] (<)
        print $ check' [9,7,6,2,1] (>)
        print $ check' [1,3,2,4,5] (<)
        print $ check' [8,6,4,4,1] (>)
        print $ check' [1,3,6,7,9] (<)
        hClose handle

checkList :: [[Int]] -> Int -> ([Int] -> (Int -> Int -> Bool) -> Bool) -> Int
checkList [] acc _ = acc
checkList (x:xs) acc f = do
    let cmp = incrdecr x
    let nexacc = (if f x cmp then 1 else 0 ) + acc
    checkList xs nexacc f


incrdecr :: [Int] -> (Int -> Int -> Bool)
incrdecr (x:y:ys) | x > y = (>)
                  | x < y = (<)
                  | x == y = incrdecr (y:ys)

check :: [Int] -> (Int -> Int -> Bool) -> Bool
check [_] _ = True
check (x:y:xs) cmp = do
    let diff = abs $ x - y
    not (diff < 1 || diff > 3) && cmp x y && check (y:xs) cmp 


check' :: [Int] -> (Int -> Int -> Bool) -> Bool
check' [_] _ = True
check' (x:y:z:xs) cmp = do
    let diff1 = abs $ x - y
    let diff2 = abs $ x - z
    
    if (diff1 < 1 || diff1 > 3) || not (diff2 < 1 || diff2 > 3) then
        if (diff2 < 1 || diff2 > 3) then
            False
        else
            cmp x z && check' (z:xs) cmp
    else
        cmp x y && check' (y:z:xs) cmp
check' (x:y:xs) cmp = do
    let diff = abs $ x - y
    not (diff < 1 || diff > 3) && cmp x y && check' (y:xs) cmp 