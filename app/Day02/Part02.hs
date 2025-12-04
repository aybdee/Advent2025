module Day02.Part02 where

import Data.Ix (inRange)
import Utils (partitionN, splitBy)

run :: IO ()
run = do
  content <- readFile "../data/Day02.txt"
  print (part1 content)

part1 :: String -> Int
part1 input =
  sum
    ( concatMap
        (filter isMultiSymmetrical . (\(start, end) -> [start .. end]))
        (parseRange input)
    )

parseRange :: String -> [(Int, Int)]
parseRange s =
  map
    ((\(start : end : _) -> (read start, read end)) . splitBy '-')
    (splitBy ',' s)

isMultiSymmetrical :: Int -> Bool
isMultiSymmetrical n =
  any
    ( \partLength ->
        let partitions = partitionN (length numText `div` partLength) numText
         in all (== head partitions) partitions
    )
    [1 .. (length numText `div` 2)]
 where
  numText = show n
