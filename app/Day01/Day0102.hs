module Main where

import Data.List ( sortOn )
import Data.List.Split ( splitOn )
import Data.Function ( (&) ) 
import Data.Ord ( Down(Down) )

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
    & sortOn Down
    & take 3
    & sum
    & print