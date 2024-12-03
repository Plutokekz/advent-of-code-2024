import Data.Char
import System.IO

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn "Part One:"
  print $ mul contents 0
  putStrLn "Part Two:"
  print $ mul' contents 0 True
  hClose handle

mul :: String -> Int -> Int
mul [] acc = acc
mul ('m' : 'u' : 'l' : '(' : xs) acc =
  case digit xs "" of
    (rest, Just a) ->
      case digit rest "" of
        (rest2, Just b) -> mul rest2 (acc + (a * b))
        (rest2, Nothing) -> mul rest2 acc
    (rest, Nothing) -> mul rest acc
mul (x : xs) acc = mul xs acc

mul' :: String -> Int -> Bool -> Int
mul' [] acc _ = acc
mul' ('d' : 'o' : '(' : ')' : xs) acc action = mul' xs acc True
mul' ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) acc action = mul' xs acc False
mul' ('m' : 'u' : 'l' : '(' : xs) acc action =
  case digit xs "" of
    (rest, Just a) ->
      case digit rest "" of
        (rest2, Just b) -> mul' rest2 (acc + (if action then a * b else 0)) action
        (rest2, Nothing) -> mul' rest2 acc action
    (rest, Nothing) -> mul' rest acc action
mul' (x : xs) acc action = mul' xs acc action

digit :: String -> String -> (String, Maybe Int)
digit (',' : xs) acc = (xs, Just (read acc :: Int))
digit (')' : xs) acc = (xs, Just (read acc :: Int))
digit (x : xs) acc
  | isDigit x = digit xs (acc ++ [x])
  | otherwise = (x : xs, Nothing)
