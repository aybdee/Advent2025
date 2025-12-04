module Day04.Part02 where

import Data.Char (digitToInt)
import Data.List (permutations)
import qualified Data.Vector as V
import Utils (countChar)

runDay4Part2 :: IO ()
runDay4Part2 = do
    content <- readFile "./data/Day04.txt"
    let input = lines content
    print (part2 input)

type Grid = V.Vector (V.Vector Char)

parseGrid :: [String] -> Grid
parseGrid = V.fromList . map V.fromList

part2 :: [String] -> Int
part2 n = removeAllForklifts (parseGrid n) 0

removeAllForklifts :: Grid -> Int -> Int
removeAllForklifts grid currentCount
    | null removeableForklifts = currentCount
    | otherwise =
        currentCount
            + removeAllForklifts
                (foldl removeForkLift grid removeableForklifts)
                currentCount
            + length removeableForklifts
  where
    removeableForklifts =
        filter
            (\coords -> getItem grid coords == '@' && countChar (getNeighbours grid coords) '@' < 4)
            ([(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (grid V.! 0) - 1]])

getItem :: Grid -> (Int, Int) -> Char
getItem grid (x, y) = (grid V.! x) V.! y

removeForkLift :: Grid -> (Int, Int) -> Grid
removeForkLift grid (x, y) =
    grid V.// [(x, newRow)]
  where
    row = grid V.! x
    newRow = row V.// [(y, '.')]

getNeighbours :: Grid -> (Int, Int) -> String
getNeighbours grid (x, y) =
    map
        (getItem grid)
        ( filter
            filterValidPositions
            [(xGen, yGen) | xGen <- [x, x + 1, x - 1], yGen <- [y, y + 1, y - 1]]
        )
  where
    filterValidPositions (xGen, yGen) =
        ( xGen /= x
            || yGen /= y
        )
            && xGen >= 0
            && yGen >= 0
            && xGen < length grid
            && yGen < length (grid V.! 0)
