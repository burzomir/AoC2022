module Main where

import Data.List
import Data.List.Split
import Data.List.Index
import Data.Function 
import Data.Bifunctor

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  input <- readFile "app/Day01/Input.txt"
  input
    & lines
    & splitOn [""]
    & (map . map) readInt
    & map sum
    & indexed
    & maximumBy (compare `on` snd)
    & first (+1)
    & print