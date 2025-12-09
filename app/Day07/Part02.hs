module Day07.Part02 where

import Utils (traceExpr)

runDay7Part1 :: IO ()
runDay7Part1 = do
  content <- readFile "./data/Day07.txt"
  let input = lines content
  print (part1 input)

-- part1 :: [String] -> Int
part1 input = scanl countAndSplitTacheons startTacheons splitters
 where
  splitters = [map (== '^') x | (i, x) <- zip [0 ..] (tail input), odd i]
  startTacheons = map (== 'S') (head input)

countAndSplitTacheons :: [Bool] -> [Bool] -> [Bool]
countAndSplitTacheons currentTacheons splitterMap =
  zipWith (||) (zipWith (&&) (map not triggerMap) currentTacheons) triggerResult
 where
  triggerMap = zipWith (&&) currentTacheons splitterMap
  triggerResult = head triggerMap : map (\index -> triggerMap !! (index + 1) || triggerMap !! (index - 1)) [1 .. length currentTacheons - 2] ++ [last triggerMap]
