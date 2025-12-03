import Data.Char (digitToInt, intToDigit)

main :: IO ()
main = do
  content <- readFile "../../data/Day03.txt"
  let input = lines content
  print (part2 input)

parseBank :: String -> [Int]
parseBank = map digitToInt

part2 :: [String] -> Int
part2 n = sum (map ((read . map intToDigit) . getLargestJoltage 12 . parseBank) n)

getLargestJoltageStep :: [Int] -> Int -> [Int]
getLargestJoltageStep [] _ = []
getLargestJoltageStep [x] n = [max x n]
getLargestJoltageStep currLongest@(first : second : rest) newDigit
  | second > first = (second : rest) ++ [newDigit]
  | otherwise = first : getLargestJoltageStep (second : rest) newDigit

getLargestJoltage :: Int -> [Int] -> [Int]
getLargestJoltage k arr = foldl getLargestJoltageStep (take k arr) (drop k arr)
