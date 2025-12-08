module Day05.Part01 where

import Data.List (partition, sortOn)
import Utils (splitBy)

runDay5Part1 :: IO ()
runDay5Part1 = do
  content <- readFile "./data/Day05.txt"
  let input = lines content
  print (part1 input)

part1 :: [String] -> Int
part1 input = length (filter (\id -> or (map isInRange ranges <*> [id])) ids)
 where
  ranges = mergeRanges (sortRanges (map parseRange (takeWhile (not . null) input)))
  ids = map read (tail (dropWhile (not . null) input))

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
