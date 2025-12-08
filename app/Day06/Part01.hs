module Day06.Part01 where

import Data.List (partition, sortOn, transpose)
import Utils (splitBy)

runDay6Part1 :: IO ()
runDay6Part1 = do
  content <- readFile "./data/Day06.txt"
  let input = lines content
  print (part1 input)

part1 :: [String] -> Int
part1 input = sum (zipWith (curry operate) operands operations)
 where
  operands =
    map
      (map read)
      (transpose (map words (init input)))
  operations =
    map
      (\op -> if op == "*" then (*) else (+))
      (words (last input))
  operate (currentOperands, currentOperation) =
    foldl1 currentOperation currentOperands
