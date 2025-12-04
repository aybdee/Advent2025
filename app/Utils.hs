module Utils (splitBy, trim, partitionN, countChar) where

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
