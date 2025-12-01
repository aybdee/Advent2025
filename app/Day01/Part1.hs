import Data.Ix (inRange)

main :: IO ()
main = do
  content <- readFile "../../data/Day01.txt"
  let input = lines content
  print (part1 input)

data Direction = L | R
  deriving (Show, Eq, Enum, Bounded)

part1 :: [String] -> Int
part1 n =
  length (filter (== 0) (getPositions (map parseRotation n)))

parseRotation :: String -> (Direction, Int)
parseRotation [] = error "cant parse empty"
parseRotation (s : xs) = (direction, read xs)
 where
  direction
    | s == 'L' = L
    | s == 'R' = R
    | otherwise = error "unexpected direction"

getPositions :: [(Direction, Int)] -> [Int]
getPositions = scanl f 50
 where
  f current (direction, magnitude)
    | direction == L = (current - magnitude) `mod` 100
    | direction == R = (current + magnitude) `mod` 100
