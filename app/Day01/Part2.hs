import Data.Ix (inRange)

main :: IO ()
main = do
  content <- readFile "../../data/Day01.txt"
  let input = lines content
  print (part2 input)

data Direction = L | R
  deriving (Show, Eq, Enum, Bounded)

part2 :: [String] -> Int
part2 n =
  sum (map snd (getPositionsAndZeroCount (map parseRotation n)))

parseRotation :: String -> (Direction, Int)
parseRotation [] = error "cant parse empty"
parseRotation (s : xs) = (direction, read xs)
 where
  direction
    | s == 'L' = L
    | s == 'R' = R
    | otherwise = error "unexpected direction"

getPositionsAndZeroCount :: [(Direction, Int)] -> [(Int, Int)]
getPositionsAndZeroCount = scanl f (50, 0)
 where
  f (current, _) (direction, magnitude) =
    let
      position = case direction of
        L -> (current - magnitude) `mod` 100
        R -> (current + magnitude) `mod` 100

      touchesZero = case direction of
        R ->
          let fullCycles = magnitude `div` 100
              remainder = magnitude `mod` 100
              extra = if current + remainder >= 100 then 1 else 0
           in fullCycles + extra
        L ->
          let fullCycles = magnitude `div` 100
              remainder = magnitude `mod` 100
              extra = if current /= 0 && remainder >= current then 1 else 0
           in fullCycles + extra
     in
      (position, touchesZero)
