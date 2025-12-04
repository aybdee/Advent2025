module Day04.Part01 where

import Data.Char (digitToInt)
import Data.List (permutations)
import qualified Data.Vector as V
import Utils (countChar)

runDay4Part1 :: IO ()
runDay4Part1 = do
    content <- readFile "./data/Day04.txt"
    let input = lines content
    print (part1 input)

type Grid = V.Vector (V.Vector Char)

getItem :: Grid -> (Int, Int) -> Char
getItem grid (x, y) = (grid V.! x) V.! y

parseGrid :: [String] -> Grid
parseGrid = V.fromList . map V.fromList

part1 :: [String] -> Int
part1 n =
    length
        ( filter
            (\neighbours -> countChar neighbours '@' < 4)
            ( map
                (getNeighbours grid)
                ( filter
                    (\coords -> getItem grid coords == '@')
                    ([(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (grid V.! 0) - 1]])
                )
            )
        )
  where
    grid = parseGrid n

getNeighbours :: Grid -> (Int, Int) -> [Char]
getNeighbours grid (x, y) =
    map
        (getItem grid)
        ( filter
            filterValidPositions
            [(xGen, yGen) | xGen <- [x, x + 1, x - 1], yGen <- [y, y + 1, y - 1]]
        )
  where
    filterValidPositions (xGen, yGen) =
        ( (xGen /= x)
            || (yGen /= y)
        )
            && (xGen >= 0)
            && (yGen >= 0)
            && (xGen < length grid)
            && (yGen < length (grid V.! 0))
