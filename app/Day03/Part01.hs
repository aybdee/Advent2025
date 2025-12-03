import Data.Char (digitToInt)

main :: IO ()
main = do
  content <- readFile "../data/Day03.txt"
  let input = lines content
  print (part1 input)

parseBank :: String -> [Int]
parseBank = map digitToInt

part1 :: [String] -> Int
part1 n = sum (map ((\(f, s) -> f * 10 + s) . getLargestJoltage . parseBank) n)

getLargestJoltage :: [Int] -> (Int, Int)
getLargestJoltage (first : second : rest) = getLargestJoltageStep (first, second) rest

--
getLargestJoltageStep :: (Int, Int) -> [Int] -> (Int, Int)
getLargestJoltageStep curr [] = curr
getLargestJoltageStep (f, s) [last]
  | s > f = (s, last)
  | last > s = (f, last)
  | otherwise = (f, s)
getLargestJoltageStep (f, s) (curr : rest)
  | s > f = getLargestJoltageStep (s, curr) rest
  | curr > f = getLargestJoltageStep (curr, 0) rest
  | curr > s = getLargestJoltageStep (f, curr) rest
  | otherwise = getLargestJoltageStep (f, s) rest
