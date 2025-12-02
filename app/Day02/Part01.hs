import Data.Ix (inRange)
import Utils (splitBy)

main :: IO ()
main = do
  content <- readFile "../data/Day02.txt"
  print (part1 content)

part1 :: String -> Int
part1 input =
  sum
    ( concatMap
        (filter isSymmetrical . (\(start, end) -> [start .. end]))
        (parseRange input)
    )

parseRange :: String -> [(Int, Int)]
parseRange s =
  map
    ((\(start : end : _) -> (read start, read end)) . splitBy '-')
    (splitBy ',' s)

isSymmetrical :: Int -> Bool
isSymmetrical n =
  even (length numText)
    && ( let (firstHalf, secondHalf) = splitAt (length numText `div` 2) numText
          in firstHalf == secondHalf
       )
 where
  numText = show n
