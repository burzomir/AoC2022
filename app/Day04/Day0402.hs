module Main where

import Text.Parsec
import Helpers
import Data.List as List

newtype Range = Range [Int] deriving (Show)
data Overlap = NoOverlap | Overlap  deriving (Show, Eq)
newtype Pair = Pair (Range, Range) deriving (Show)

overlap :: Range -> Range -> Overlap
overlap (Range r1) (Range r2) = 
    let intersection = List.intersect r1 r2
    in if length intersection > 0 
        then Overlap 
        else NoOverlap

rangeParser :: Parsec String () Range
rangeParser = do
  start <- many digit
  char '-'
  end <- many digit
  return $ Range [read start .. read end]

pairParser :: Parsec String () Pair
pairParser = do
  range1 <- rangeParser
  char ','
  range2 <- rangeParser
  return $ Pair (range1, range2)

fileParser :: Parsec String () [Pair]
fileParser = do
  pairs <- pairParser `sepBy` endOfLine
  eof
  return pairs

hasOverlap :: Pair -> Bool
hasOverlap (Pair (r1, r2)) = overlap r1 r2 == Overlap

main = do
    input <- parseFile fileParser "app/Day04/Input.txt"
    print $ fmap (length . List.filter hasOverlap) input
    
     




