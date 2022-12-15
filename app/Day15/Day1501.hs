module Main where

import Text.Parsec
import File
import Data.Maybe
import Prelude hiding (length)
import Data.List (sort)

type Position = (Int, Int)
type SensorPosition = Position
type BeaconPosition = Position
type SensorRadius = Int
type Sensor = (SensorPosition, BeaconPosition)

intParser :: Parsec String () Int
intParser = do
  sign <- optionMaybe (char '-')
  digits <- many1 digit
  return $ read (maybe "" (const "-") sign ++ digits)

positionParser :: Parsec String () Position
positionParser = do
  _ <- string "x="
  x <- intParser
  _ <- string ", y="
  y <- intParser
  return (x, y)

sensorParser :: Parsec String () Sensor
sensorParser = do
  _ <- string "Sensor at "
  sensorPosition <- positionParser
  _ <- string ": closest beacon is at "
  beaconPosition <- positionParser
  return (sensorPosition, beaconPosition)

inputParser :: Parsec String () [Sensor]
inputParser = do
  sensorParser `sepBy` endOfLine

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

type Range = (Int, Int)

rangeAtLine :: Int -> Sensor -> Maybe Range
rangeAtLine line (sensorPosition@(x, y), beaconPosition) =
  let d = distance sensorPosition beaconPosition
      minX = x - d
      maxX = x + d
      lineDistance = abs (y - line)
      r1 =  minX + lineDistance
      r2 = maxX - lineDistance
  in if lineDistance > d
     then Nothing
     else Just (r1, r2)

overlap :: Range -> Range -> [Range]
overlap (r1a, r1b) (r2a, r2b) = 
  if r1a <= r2b && r2a <= r1b
    then [(min r1a r2a, max r1b r2b)]
    else [(r1a, r1b), (r2a, r2b)]

overlap' :: [Range] -> [Range]
overlap' [] = []
overlap' [r] = [r]
overlap' (r1:r2:rs) = 
  case overlap r1 r2 of
    [r] -> overlap' (r:rs)
    [r1', r2'] -> r1' : overlap' (r2':rs)
    _ -> error "overlap'"

overlap'' :: [Range] -> [Range]
overlap'' ranges = overlap' $ sort ranges

length :: Range -> Int
length (a, b) = b - a

main :: IO ()
main = do
    input <- parseFile inputParser "app/Day15/Input.txt"
    -- print $ fmap (fmap length . overlap'' . catMaybes) $ (fmap . fmap) (rangeAtLine 10) input
    print $ fmap (fmap length . overlap'' . catMaybes) $ (fmap . fmap) (rangeAtLine 2000000) input

