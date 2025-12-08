module Day05.Part02 where

import Data.List (partition, sortOn)
import Utils (splitBy)

runDay5Part2 :: IO ()
runDay5Part2 = do
  content <- readFile "./data/Day05.txt"
  let input = lines content
  print (part2 input)

part2 :: [String] -> Int
part2 input = foldl sumRanges 0 ranges
 where
  ranges = mergeRanges (sortRanges (map parseRange (takeWhile (not . null) input)))
  sumRanges currSum (start, end) = currSum + ((end - start) + 1)

parseRange :: String -> (Int, Int)
parseRange n = let [a, b] = splitBy '-' n in (read a, read b)

sortRanges :: [(Int, Int)] -> [(Int, Int)]
sortRanges = sortOn fst

isInRange :: (Int, Int) -> Int -> Bool
isInRange (start, end) n = n >= start && n <= end

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ranges = reverse (foldl f [head ranges] (tail ranges))
 where
  f acc@((startAcc, endAcc) : rest) curr@(startCurr, endCurr)
    | isInRange (startAcc, endAcc + 1) startCurr = (startAcc, max endAcc endCurr) : rest
    | otherwise = curr : acc
