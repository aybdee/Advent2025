module Utils (splitBy, trim, partitionN, countChar, splitAtIndices, replaceChar) where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, isSuffixOf, stripPrefix)
import Debug.Trace (trace)

splitBy delimeter = foldr f [[]]
 where
  f c l@(h : t)
    | c == delimeter = [] : l
    | otherwise = (c : h) : t

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

traceExpr x = trace (show x) x

partitionN :: Int -> [a] -> [[a]]
partitionN 0 _ = error "cannot partition into 0 parts"
partitionN _ [] = []
partitionN numDivisions arr = reverse (foldl f [[]] arr)
 where
  f (x : xs) curr =
    if length x < (length arr `div` numDivisions)
      then (x ++ [curr]) : xs
      else [curr] : x : xs

countChar :: [Char] -> Char -> Int
countChar str char = length (filter (== char) str)

replaceChar :: Char -> Char -> String -> String
replaceChar old new = map (\c -> if c == old then new else c)

splitAtIndices :: [Int] -> [a] -> [[a]]
splitAtIndices [] xs = [xs]
splitAtIndices (i : is) xs =
  let (first, rest) = splitAt i xs
      rest' = drop 1 rest
      is' = map (\x -> x - (i + 1)) is
   in first : splitAtIndices is' rest'
