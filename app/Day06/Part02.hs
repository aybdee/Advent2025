module Day06.Part02 where

import Data.List (partition, sortOn, transpose)
import Utils (replaceChar, splitAtIndices, splitBy)

runDay6Part2 :: IO ()
runDay6Part2 = do
  content <- readFile "./data/Day06.txt"
  let input = lines content
  print (part2 input)

part2 :: [String] -> Int
part2 input = sum (zipWith operate operands operations)
 where
  operands =
    map
      (map (read . filter (/= '-')) . transpose . map (replaceChar ' ' '-'))
      (transpose (parseNumbers (init input)))
  operations =
    map
      (\op -> if op == "*" then (*) else (+))
      (words (last input))
  operate currentOperands currentOperation =
    foldl1 currentOperation currentOperands

parseNumbers :: [String] -> [[String]]
parseNumbers n = map (splitAtIndices (filter isBreak [0 .. length (head n) - 1])) n
 where
  isBreak index = all ((== ' ') . (!! index)) n
